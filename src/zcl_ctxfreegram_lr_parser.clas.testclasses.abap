*"* use this source file for your ABAP unit test classes

CLASS ltc_errors DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS unexpected_token FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_parse_optional_complex DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS parse_with_option FOR TESTING RAISING cx_static_check.
    METHODS parse_without_option FOR TESTING RAISING cx_static_check.

    METHODS setup.

    DATA grammar TYPE REF TO zcl_ctxfreegram_grammar.
    DATA parser TYPE REF TO zcl_ctxfreegram_lr_parser.

ENDCLASS.

CLASS ltc_parse_optional_simple DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS parse_not_empty FOR TESTING RAISING cx_static_check.
    METHODS parse_empty FOR TESTING RAISING cx_static_check.

    METHODS setup.

    DATA grammar TYPE REF TO zcl_ctxfreegram_grammar.
    DATA parser TYPE REF TO zcl_ctxfreegram_lr_parser.

ENDCLASS.

CLASS ltc_tokenizer DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS simple FOR TESTING RAISING cx_static_check.
    METHODS regex_non_space FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltc_wikipedia_example DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS parse_1_plus_0 FOR TESTING RAISING cx_static_check.
    METHODS parse_1_plus_1_plus_1 FOR TESTING RAISING cx_static_check.

    METHODS setup.

    DATA grammar TYPE REF TO zcl_ctxfreegram_grammar.
    DATA parser TYPE REF TO zcl_ctxfreegram_lr_parser.

ENDCLASS.

CLASS lth_string_table_utilities DEFINITION
    FINAL
    CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS convert_without_trailing_blank
      IMPORTING
        string_table  TYPE string_table
      RETURNING
        VALUE(result) TYPE string_table.
ENDCLASS.

CLASS ltc_errors IMPLEMENTATION.

  METHOD unexpected_token.

    DATA(grammar) = zcl_ctxfreegram_grammar=>create_from_string_table(
                start_rule   = 'start'
                string_table = VALUE #(
                    ( `start: 'a' 'b' ` ) ) ).

    DATA(parser) = zcl_ctxfreegram_lr_parser=>create(
        io_context_free_grammar = grammar
        record_trace            = abap_true ).

    TRY.
        parser->parse( zcl_ctxfreegram_tokenizer_1c=>create_from_string( string = `b` ) ).
      CATCH zcx_ctxfreegram_lr_parser INTO DATA(error).
    ENDTRY.

    cl_abap_unit_assert=>assert_bound( act = error
                                       msg = 'Expected error but no exception raised' ).
    cl_abap_unit_assert=>assert_equals( act = error->error
                                        exp = zcx_ctxfreegram_lr_parser=>c_error-parse_unexpected_token ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_parse_optional_complex IMPLEMENTATION.

  METHOD parse_with_option.

    parser->parse( zcl_ctxfreegram_tokenizer_1c=>create_from_string( string = 'abc' ) ).

    DATA(act) = zcl_ctxfreegram_utilities=>render_table( parser->get_ast_as_string_table( with_rule_number = abap_true ) ).

    DATA(exp) = `start: 'a' optional 'c' (#1) :   ` && |\n|
             && `    Offset 0, length 1: "a"      ` && |\n|
             && `    optional: 'b' (#2) :         ` && |\n|
             && `        Offset 1, length 1: "b"  ` && |\n|
             && `    Offset 2, length 1: "c"      ` && |\n|.

    cl_abap_unit_assert=>assert_equals( act = act
                                        exp = exp ).
  ENDMETHOD.

  METHOD parse_without_option.

    parser->parse( zcl_ctxfreegram_tokenizer_1c=>create_from_string( string = 'ac' ) ).

    DATA(act) = zcl_ctxfreegram_utilities=>render_table( parser->get_ast_as_string_table( with_rule_number = abap_true ) ).

    DATA(exp) = `start: 'a' optional 'c' (#1) :  ` && |\n|
             && `    Offset 0, length 1: "a"     ` && |\n|
             && `    optional:  (#3) :           ` && |\n|
             && `    Offset 1, length 1: "c"     ` && |\n|.

    cl_abap_unit_assert=>assert_equals( act = act
                                        exp = exp ).
  ENDMETHOD.

  METHOD setup.

    grammar = zcl_ctxfreegram_grammar=>create_from_string_table(
                start_rule   = 'start'
                string_table = VALUE #(
                    ( `start: 'a' optional 'c'` )
                    ( `optional: 'b'  ` )
                    ( `optional:      ` ) ) ).

    parser = zcl_ctxfreegram_lr_parser=>create(
        io_context_free_grammar = grammar
        record_trace            = abap_true ).

  ENDMETHOD.

ENDCLASS.

CLASS ltc_parse_optional_simple IMPLEMENTATION.

  METHOD parse_empty.

    parser->parse( zcl_ctxfreegram_tokenizer_1c=>create_from_string( string = '' ) ).

    DATA(act) = zcl_ctxfreegram_utilities=>render_table( parser->get_ast_as_string_table( with_rule_number = abap_true ) ).

    DATA(exp) = `start: optional (#1) :  ` && |\n|
             && `    optional:  (#3) :   ` && |\n|.

    cl_abap_unit_assert=>assert_equals( act = act
                                        exp = exp ).
  ENDMETHOD.

  METHOD parse_not_empty.

    parser->parse( zcl_ctxfreegram_tokenizer_1c=>create_from_string( string = 'a' ) ).

    DATA(act) = zcl_ctxfreegram_utilities=>render_table( parser->get_ast_as_string_table( with_rule_number = abap_true ) ).

    DATA(exp) = `start: optional (#1) :           ` && |\n|
             && `    optional: 'a' (#2) :         ` && |\n|
             && `        Offset 0, length 1: "a"  ` && |\n|.

    cl_abap_unit_assert=>assert_equals( act = act
                                        exp = exp ).
  ENDMETHOD.

  METHOD setup.

    grammar = zcl_ctxfreegram_grammar=>create_from_string_table(
                start_rule   = 'start'
                string_table = VALUE #(
                    ( `start: optional` )
                    ( `optional: 'a'  ` )
                    ( `optional:      ` ) ) ).

    parser = zcl_ctxfreegram_lr_parser=>create(
        io_context_free_grammar = grammar
        record_trace            = abap_true ).

  ENDMETHOD.

ENDCLASS.

CLASS ltc_tokenizer IMPLEMENTATION.

  METHOD regex_non_space.

    DATA(grammar) = zcl_ctxfreegram_grammar=>create_from_string_table(
                        start_rule   = 'start'
                        string_table = VALUE #(
                            ( `start: tokens` )
                            ( `tokens: tokens token` )
                            ( `tokens: token` )
                            ( `token: nonspaces` )
                            ( `token: spaces` )
                            ( `nonspaces: nonspaces nonspace` )
                            ( `nonspaces: nonspace` )
                            ( `spaces: spaces space` )
                            ( `spaces: space` )
                            ( `nonspace: [^ ]` )
                            ( `space: ' '` ) ) ).

    DATA(parser) = zcl_ctxfreegram_lr_parser=>create(
        io_context_free_grammar = grammar
        record_trace            = abap_true ).

    TRY.
        parser->parse( zcl_ctxfreegram_tokenizer_1c=>create_from_string( string = ` ab  cd ` ) ).
      CATCH zcx_ctxfreegram INTO DATA(error).

    ENDTRY.

    DATA(act) = zcl_ctxfreegram_utilities=>render_table( parser->get_ast_as_string_table( ) ).

    DATA(exp) = `` && |\n|
             && `` && |\n|
             && `` && |\n|.

    cl_abap_unit_assert=>assert_equals( act = act
                                        exp = exp ).
  ENDMETHOD.

  METHOD simple.

    DATA(grammar) = zcl_ctxfreegram_grammar=>create_from_string_table(
                        start_rule   = 'start'
                        string_table = VALUE #(
                            ( `start: tokens` )
                            ( `tokens: tokens token` )
                            ( `tokens: token` )
                            ( `token: nonspaces` )
                            ( `token: spaces` )
                            ( `nonspaces: nonspaces nonspace` )
                            ( `nonspaces: nonspace` )
                            ( `spaces: spaces space` )
                            ( `spaces: space` )
                            ( `nonspace: 'x'` )
                            ( `space: ' '` ) ) ).

    DATA(parser) = zcl_ctxfreegram_lr_parser=>create(
        io_context_free_grammar = grammar
        record_trace            = abap_true ).

    TRY.
        parser->parse( zcl_ctxfreegram_tokenizer_1c=>create_from_string( string = ` xx  xx ` ) ).
      CATCH zcx_ctxfreegram INTO DATA(error).

    ENDTRY.

    DATA(act) = zcl_ctxfreegram_utilities=>render_table( parser->get_ast_as_string_table( ) ).

    DATA(exp) = `start: tokens                                                ` && |\n|
             && `    tokens: tokens token                                     ` && |\n|
             && `        tokens: tokens token                                 ` && |\n|
             && `            tokens: tokens token                             ` && |\n|
             && `                tokens: tokens token                         ` && |\n|
             && `                    tokens: token                            ` && |\n|
             && `                        token: spaces                        ` && |\n|
             && `                            spaces: space                    ` && |\n|
             && `                                space: ' '                   ` && |\n|
             && `                                    Offset 0, length 1: " "  ` && |\n|
             && `                    token: nonspaces                         ` && |\n|
             && `                        nonspaces: nonspaces nonspace        ` && |\n|
             && `                            nonspaces: nonspace              ` && |\n|
             && `                                nonspace: 'x'                ` && |\n|
             && `                                    Offset 1, length 1: "x"  ` && |\n|
             && `                            nonspace: 'x'                    ` && |\n|
             && `                                Offset 2, length 1: "x"      ` && |\n|
             && `                token: spaces                                ` && |\n|
             && `                    spaces: spaces space                     ` && |\n|
             && `                        spaces: space                        ` && |\n|
             && `                            space: ' '                       ` && |\n|
             && `                                Offset 3, length 1: " "      ` && |\n|
             && `                        space: ' '                           ` && |\n|
             && `                            Offset 4, length 1: " "          ` && |\n|
             && `            token: nonspaces                                 ` && |\n|
             && `                nonspaces: nonspaces nonspace                ` && |\n|
             && `                    nonspaces: nonspace                      ` && |\n|
             && `                        nonspace: 'x'                        ` && |\n|
             && `                            Offset 5, length 1: "x"          ` && |\n|
             && `                    nonspace: 'x'                            ` && |\n|
             && `                        Offset 6, length 1: "x"              ` && |\n|
             && `        token: spaces                                        ` && |\n|
             && `            spaces: space                                    ` && |\n|
             && `                space: ' '                                   ` && |\n|
             && `                    Offset 7, length 1: " "                  ` && |\n|.

    cl_abap_unit_assert=>assert_equals( act = act
                                        exp = exp ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_wikipedia_example IMPLEMENTATION.

  METHOD setup.

    grammar = zcl_ctxfreegram_grammar=>create_from_string_table(
                start_rule   = 'start'
                string_table = VALUE #(
                    ( `start: E  ` )
                    ( `E: E '*' B` )
                    ( `E: E '+' B` )
                    ( `E: B      ` )
                    ( `B: '0'    ` )
                    ( `B: '1'    ` ) ) ).

    parser = zcl_ctxfreegram_lr_parser=>create(
        io_context_free_grammar = grammar
        record_trace            = abap_true ).

  ENDMETHOD.

  METHOD parse_1_plus_0.

    parser->parse( zcl_ctxfreegram_tokenizer_1c=>create_from_string( string = '1+0' ) ).

    DATA(act) = zcl_ctxfreegram_utilities=>render_table( parser->get_ast_as_string_table( ) ).

    DATA(exp) = `start: E                                 ` && |\n|
             && `    E: E '+' B                           ` && |\n|
             && `        E: B                             ` && |\n|
             && `            B: '1'                       ` && |\n|
             && `                Offset 0, length 1: "1"  ` && |\n|
             && `        Offset 1, length 1: "+"          ` && |\n|
             && `        B: '0'                           ` && |\n|
             && `            Offset 2, length 1: "0"      ` && |\n|.

    cl_abap_unit_assert=>assert_equals( act = act
                                        exp = exp ).

  ENDMETHOD.

  METHOD parse_1_plus_1_plus_1.

    parser->parse( zcl_ctxfreegram_tokenizer_1c=>create_from_string( string = '1+1+1' ) ).

    DATA(act) = zcl_ctxfreegram_utilities=>render_table( parser->get_ast_as_string_table( ) ).

    DATA(exp) = `start: E                                     ` && |\n|
             && `    E: E '+' B                               ` && |\n|
             && `        E: E '+' B                           ` && |\n|
             && `            E: B                             ` && |\n|
             && `                B: '1'                       ` && |\n|
             && `                    Offset 0, length 1: "1"  ` && |\n|
             && `            Offset 1, length 1: "+"          ` && |\n|
             && `            B: '1'                           ` && |\n|
             && `                Offset 2, length 1: "1"      ` && |\n|
             && `        Offset 3, length 1: "+"              ` && |\n|
             && `        B: '1'                               ` && |\n|
             && `            Offset 4, length 1: "1"          ` && |\n|.

    cl_abap_unit_assert=>assert_equals( act = act
                                        exp = exp ).
  ENDMETHOD.

ENDCLASS.

CLASS lth_string_table_utilities IMPLEMENTATION.
  METHOD convert_without_trailing_blank.
    result = string_table.
    LOOP AT result REFERENCE INTO DATA(line).
      cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = line->* ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
