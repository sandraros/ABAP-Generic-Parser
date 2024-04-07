*"* use this source file for your ABAP unit test classes

CLASS ltc_main DEFINITION DEFERRED.
CLASS zcl_ctxfreegram_grammar DEFINITION LOCAL FRIENDS ltc_main.

CLASS ltc_create_from_string_table DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS wikipedia_example FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS lhs_not_bound FOR TESTING RAISING cx_static_check.
    METHODS missing_start_rule FOR TESTING RAISING cx_static_check.
    METHODS optional FOR TESTING RAISING cx_static_check.
    METHODS optional_b_in_abc FOR TESTING RAISING cx_static_check.
    METHODS rhs_nonterminal_wo_lhs FOR TESTING RAISING cx_static_check.
    METHODS start_rule_twice FOR TESTING RAISING cx_static_check.
    METHODS unique_rule_one_character FOR TESTING RAISING cx_static_check.
    METHODS unique_rule_two_characters FOR TESTING RAISING cx_static_check.
    METHODS while_and_regex FOR TESTING RAISING cx_static_check.
    METHODS wikipedia_example FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_tokenizer DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_create_from_string_table IMPLEMENTATION.

  METHOD wikipedia_example.

    DATA(factory) = zcl_ctxfreegram_factory=>create( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    DATA(e) = factory->new_nonterminal( 'E' ).
    DATA(b) = factory->new_nonterminal( 'B' ).
    DATA(star) = factory->new_terminal( '*' ).
    DATA(plus) = factory->new_terminal( '+' ).
    DATA(_0) = factory->new_terminal( '0' ).
    DATA(_1) = factory->new_terminal( '1' ).

    DATA(grammar) = zcl_ctxfreegram_grammar=>create(
        start_rule = start
        rules      = VALUE #(
            ( lhs = start rhs = e )
            ( lhs = e     rhs = factory->new_sequence( VALUE #( ( e ) ( star ) ( b ) ) ) )
            ( lhs = e     rhs = factory->new_sequence( VALUE #( ( e ) ( plus ) ( b ) ) ) )
            ( lhs = e     rhs = b )
            ( lhs = b     rhs = _0 )
            ( lhs = b     rhs = _1 ) ) ).

    DATA(grammar_2) = zcl_ctxfreegram_grammar=>create_from_string_table(
                        start_rule   = 'start'
                        string_table = VALUE #(
                            ( `start: E  ` )
                            ( `E: E '*' B` )
                            ( `E: E '+' B` )
                            ( `E: B      ` )
                            ( `B: '0'    ` )
                            ( `B: '1'    ` ) ) ).

    DATA(exp) = |   '$'  '*'  '+'  '0'  '1'  start  E   B   \n| &&
                |1                 s2   s3          4   5   \n| &&
                |2  r5   r5   r5                            \n| &&
                |3  r6   r6   r6                            \n| &&
                |4  acc  s6   s7                            \n| &&
                |5  r4   r4   r4                            \n| &&
                |6                 s2   s3              8   \n| &&
                |7                 s2   s3              9   \n| &&
                |8  r2   r2   r2                            \n| &&
                |9  r3   r3   r3                            \n|.

    cl_abap_unit_assert=>assert_equals( act = grammar->render_action_goto_tables( )
                                        exp = exp ).

    cl_abap_unit_assert=>assert_equals( act = grammar_2->render_action_goto_tables( )
                                        exp = exp ).

  ENDMETHOD.

ENDCLASS.

CLASS ltc_main IMPLEMENTATION.

  METHOD lhs_not_bound.

    DATA(factory) = zcl_ctxfreegram_factory=>create( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    TRY.
        DATA(grammar) = zcl_ctxfreegram_grammar=>create( start_rule = start
                                                 rules      = VALUE #( ( lhs = start ) ( ) ) ).
      CATCH zcx_ctxfreegram INTO DATA(lx).
    ENDTRY.

    IF lx IS NOT BOUND OR lx->error <> zcx_ctxfreegram=>c_error-lhs_element_not_bound.
      cl_abap_unit_assert=>fail( msg = 'Exception "missing LHS" expected' ).
    ENDIF.

  ENDMETHOD.

  METHOD missing_start_rule.

    DATA(factory) = zcl_ctxfreegram_factory=>create( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    TRY.
        DATA(grammar) = zcl_ctxfreegram_grammar=>create( start_rule = start
                                                 rules      = VALUE #( ) ).
      CATCH zcx_ctxfreegram INTO DATA(lx).
    ENDTRY.

    IF lx IS NOT BOUND OR lx->error <> zcx_ctxfreegram=>c_error-missing_start_rule.
      cl_abap_unit_assert=>fail( msg = 'Exception "start rule missing" expected' ).
    ENDIF.

  ENDMETHOD.

  METHOD optional.

    " start: optional
    " optional: 'a'
    " optional:

    DATA(factory) = zcl_ctxfreegram_factory=>create( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    DATA(optional) = factory->new_nonterminal( 'optional' ).
    DATA(letter_a) = factory->new_terminal( 'a' ).
    DATA(grammar) = zcl_ctxfreegram_grammar=>create( start_rule = start
                                             rules = VALUE #(
                                                    ( lhs = start    rhs = optional )
                                                    ( lhs = optional rhs = letter_a )
                                                    ( lhs = optional ) ) ).

    DATA(first_sets) = grammar->render_first_sets( ).
    DATA(follow_sets) = grammar->render_follow_sets( ).
    DATA(item_sets) = grammar->render_item_sets( ).
    DATA(transitions) = grammar->render_transitions( ).
    DATA(action_goto_tables) = grammar->render_action_goto_tables( ).

    cl_abap_unit_assert=>assert_equals( act = item_sets exp =
        |* Item set 1:\n| &
        |** Item kernel: start = ◆ optional\n| &
        |** Item closure: optional = ◆ 'a'\n| &
        |** Item closure: optional = ◆\n| &
        |* Item set 2:\n| &
        |** Item kernel: optional = 'a' ◆\n| &
        |* Item set 3:\n| &
        |** Item kernel: start = optional ◆\n| ).
    cl_abap_unit_assert=>assert_equals( act = transitions exp =
        |   '$'  'a'  start  optional  \n| &
        |1       2           3         \n| &
        |2                             \n| &
        |3                             \n| ).
    cl_abap_unit_assert=>assert_equals( act = action_goto_tables exp =
        |   '$'  'a'  start  optional  \n| &
        |1  r3   s2          3         \n| &
        |2  r2                         \n| &
        |3  acc                        \n| ).

  ENDMETHOD.

  METHOD optional_b_in_abc.

    DATA(grammar) = zcl_ctxfreegram_grammar=>create_from_string_table(
                start_rule   = 'start'
                string_table = VALUE #(
                    ( `start: 'a' optional 'c'` )
                    ( `optional: 'b'  ` )
                    ( `optional:      ` ) ) ).

    DATA(action_goto_tables) = grammar->render_action_goto_tables( ).

    cl_abap_unit_assert=>assert_equals( act = action_goto_tables exp =
        |   '$'  'a'  'c'  'b'  start  optional  \n| &
        |1       s2                              \n| &
        |2            r3   s3          4         \n| &
        |3            r2                         \n| &
        |4            s5                         \n| &
        |5  acc                                  \n| ).

  ENDMETHOD.

  METHOD rhs_nonterminal_wo_lhs.

    TRY.
        DATA(grammar) = zcl_ctxfreegram_grammar=>create_from_string_table(
                            start_rule   = 'start'
                            string_table = VALUE #(
                                ( `start: tokens` ) ) ).
      CATCH zcx_ctxfreegram INTO DATA(lx).
    ENDTRY.

    IF lx IS NOT BOUND OR lx->error <> zcx_ctxfreegram=>c_error-rhs_nonterminal_wo_lhs.
      cl_abap_unit_assert=>fail( msg = 'Exception "One nonterminal is in RHS but is missing in LHS" expected' ).
    ENDIF.

  ENDMETHOD.

  METHOD start_rule_twice.

    DATA(factory) = zcl_ctxfreegram_factory=>create( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    TRY.
        DATA(grammar) = zcl_ctxfreegram_grammar=>create( start_rule = start
                                                 rules      = VALUE #(
                                                            ( lhs = start )
                                                            ( lhs = start ) ) ).
      CATCH zcx_ctxfreegram INTO DATA(lx).
    ENDTRY.

    IF lx IS NOT BOUND OR lx->error <> zcx_ctxfreegram=>c_error-start_rule_must_appear_once.
      cl_abap_unit_assert=>fail( msg = 'Exception "start rule must appear once" expected' ).
    ENDIF.

  ENDMETHOD.

  METHOD unique_rule_one_character.

    DATA(factory) = zcl_ctxfreegram_factory=>create( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    DATA(letter_a) = factory->new_terminal( 'a' ).
    DATA(grammar) = zcl_ctxfreegram_grammar=>create( start_rule = start
                                             rules      = VALUE #(
                                                        ( lhs = start rhs = letter_a ) ) ).

    DATA(item_sets) = grammar->render_item_sets( ).
    DATA(transitions) = grammar->render_transitions( ).
    DATA(action_goto_tables) = grammar->render_action_goto_tables( ).

    cl_abap_unit_assert=>assert_equals( act = item_sets exp =
        |* Item set 1:\n| &
        |** Item kernel: start = ◆ 'a'\n| &
        |* Item set 2:\n| &
        |** Item kernel: start = 'a' ◆\n| ).
    cl_abap_unit_assert=>assert_equals( act = transitions exp =
        |   '$'  'a'  start  \n| &
        |1       2           \n| &
        |2                   \n| ).
    cl_abap_unit_assert=>assert_equals( act = action_goto_tables exp =
        |   '$'  'a'  start  \n| &
        |1       s2          \n| &
        |2  acc              \n| ).

  ENDMETHOD.

  METHOD unique_rule_two_characters.

    " start: 'a' 'a'

    DATA(factory) = zcl_ctxfreegram_factory=>create( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    DATA(a) = factory->new_terminal( 'a' ).
    DATA(grammar) = zcl_ctxfreegram_grammar=>create( start_rule = start
                                             rules      = VALUE #(
                                                        ( lhs = start rhs = factory->new_sequence( VALUE #( ( a ) ( a ) ) ) ) ) ).

    DATA(item_sets) = grammar->render_item_sets( ).
    DATA(transitions) = grammar->render_transitions( ).
    DATA(action_goto_tables) = grammar->render_action_goto_tables( ).

    cl_abap_unit_assert=>assert_equals( act = item_sets exp =
        |* Item set 1:\n| &
        |** Item kernel: start = ◆ 'a' 'a'\n| &
        |* Item set 2:\n| &
        |** Item kernel: start = 'a' ◆ 'a'\n| &
        |* Item set 3:\n| &
        |** Item kernel: start = 'a' 'a' ◆\n| ).
    cl_abap_unit_assert=>assert_equals( act = transitions exp =
        |   '$'  'a'  start  \n| &
        |1       2           \n| &
        |2       3           \n| &
        |3                   \n| ).
    cl_abap_unit_assert=>assert_equals( act = action_goto_tables exp =
        |   '$'  'a'  start  \n| &
        |1       s2          \n| &
        |2       s3          \n| &
        |3  acc              \n| ).

  ENDMETHOD.

  METHOD while_and_regex.

    " start: while
    " while: 'while' condition
    " condition: variable '=' number
    " variable: 'regex:[a-zA-Z_][a-zA-Z_0-9]*'
    " number: 'regex:[0-9][0-9]*'

    DATA(factory) = zcl_ctxfreegram_factory=>create( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    DATA(while) = factory->new_nonterminal( 'while' ).
    DATA(condition) = factory->new_nonterminal( 'condition' ).
    DATA(_while) = factory->new_terminal( 'while' ).
    DATA(variable) = factory->new_nonterminal( 'variable' ).
    DATA(variable_regex) = factory->new_terminal_regex( '[a-zA-Z_][a-zA-Z_0-9]*' ).
    DATA(equal) = factory->new_terminal( '=' ).
    DATA(number) = factory->new_nonterminal( 'number' ).
    DATA(number_regex) = factory->new_terminal_regex( '[0-9][0-9]*' ).

    DATA(grammar) = zcl_ctxfreegram_grammar=>create(
        start_rule = start
        rules = VALUE #(
            ( lhs = start     rhs = while )
            ( lhs = while     rhs = factory->new_sequence( VALUE #( ( _while ) ( condition ) ) ) )
            ( lhs = condition rhs = factory->new_sequence( VALUE #( ( variable ) ( equal ) ( number ) ) ) )
            ( lhs = variable  rhs = variable_regex )
            ( lhs = number    rhs = number_regex ) ) ).

    DATA(item_sets) = grammar->render_item_sets( ).
    DATA(transitions) = grammar->render_transitions( ).
    DATA(action_goto_tables) = grammar->render_action_goto_tables( ).

  ENDMETHOD.

  METHOD wikipedia_example.

    " start: E
    " E: E '*' B
    " E: E '+' B
    " E: B
    " B: '0'
    " B: '1'
    DATA(factory) = zcl_ctxfreegram_factory=>create( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    DATA(e) = factory->new_nonterminal( 'E' ).
    DATA(b) = factory->new_nonterminal( 'B' ).
    DATA(star) = factory->new_terminal( '*' ).
    DATA(plus) = factory->new_terminal( '+' ).
    DATA(_0) = factory->new_terminal( '0' ).
    DATA(_1) = factory->new_terminal( '1' ).

    DATA(grammar) = zcl_ctxfreegram_grammar=>create(
        start_rule = start
        rules      = VALUE #(
            ( lhs = start rhs = e )
            ( lhs = e     rhs = factory->new_sequence( VALUE #( ( e ) ( star ) ( b ) ) ) )
            ( lhs = e     rhs = factory->new_sequence( VALUE #( ( e ) ( plus ) ( b ) ) ) )
            ( lhs = e     rhs = b )
            ( lhs = b     rhs = _0 )
            ( lhs = b     rhs = _1 ) ) ).

    cl_abap_unit_assert=>assert_equals( act = grammar->render_item_sets( ) exp =
        |* Item set 1:\n| &&
        |** Item kernel: start = ◆ E\n| &&
        |** Item closure: E = ◆ E '*' B\n| &&
        |** Item closure: E = ◆ E '+' B\n| &&
        |** Item closure: E = ◆ B\n| &&
        |** Item closure: B = ◆ '0'\n| &&
        |** Item closure: B = ◆ '1'\n| &&
        |* Item set 2:\n| &&
        |** Item kernel: B = '0' ◆\n| &&
        |* Item set 3:\n| &&
        |** Item kernel: B = '1' ◆\n| &&
        |* Item set 4:\n| &&
        |** Item kernel: start = E ◆\n| &&
        |** Item kernel: E = E ◆ '*' B\n| &&
        |** Item kernel: E = E ◆ '+' B\n| &&
        |* Item set 5:\n| &&
        |** Item kernel: E = B ◆\n| &&
        |* Item set 6:\n| &&
        |** Item kernel: E = E '*' ◆ B\n| &&
        |** Item closure: B = ◆ '0'\n| &&
        |** Item closure: B = ◆ '1'\n| &&
        |* Item set 7:\n| &&
        |** Item kernel: E = E '+' ◆ B\n| &&
        |** Item closure: B = ◆ '0'\n| &&
        |** Item closure: B = ◆ '1'\n| &&
        |* Item set 8:\n| &&
        |** Item kernel: E = E '*' B ◆\n| &&
        |* Item set 9:\n| &&
        |** Item kernel: E = E '+' B ◆\n| ).
    cl_abap_unit_assert=>assert_equals( act = grammar->render_transitions( ) exp =
        |   '$'  '*'  '+'  '0'  '1'  start  E   B   \n| &&
        |1                 2    3           4   5   \n| &&
        |2                                          \n| &&
        |3                                          \n| &&
        |4       6    7                             \n| &&
        |5                                          \n| &&
        |6                 2    3               8   \n| &&
        |7                 2    3               9   \n| &&
        |8                                          \n| &&
        |9                                          \n| ).
    " start: E
    " E: E '*' B
    " E: E '+' B
    " E: B
    " B: '0'
    " B: '1'
    cl_abap_unit_assert=>assert_equals( act = grammar->render_action_goto_tables( ) exp =
        |   '$'  '*'  '+'  '0'  '1'  start  E   B   \n| &&
        |1                 s2   s3          4   5   \n| &&
        |2  r5   r5   r5                            \n| &&
        |3  r6   r6   r6                            \n| &&
        |4  acc  s6   s7                            \n| &&
        |5  r4   r4   r4                            \n| &&
        |6                 s2   s3              8   \n| &&
        |7                 s2   s3              9   \n| &&
        |8  r2   r2   r2                            \n| &&
        |9  r3   r3   r3                            \n| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_tokenizer IMPLEMENTATION.

  METHOD test.

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

    DATA(act) = grammar->render_action_goto_tables( ).

    cl_abap_unit_assert=>assert_equals( act = grammar->render_action_goto_tables( ) exp =
        |    '$'  'x'  ' '  start  tokens  token  nonspaces  spaces  nonspace  space  \n| &&
        |1        s2   s3          4       5      6          7       8         9      \n| &&
        |2   r10  r10  r10                                                            \n| &&
        |3   r11  r11  r11                                                            \n| &&
        |4   acc  s2   s3                  10     6          7       8         9      \n| &&
        |5   r3   r3   r3                                                             \n| &&
        |6   r4   s2   r4                                            11               \n| &&
        |7   r5   r5   s3                                                      12     \n| &&
        |8   r7   r7   r7                                                             \n| &&
        |9   r9   r9   r9                                                             \n| &&
        |10  r2   r2   r2                                                             \n| &&
        |11  r6   r6   r6                                                             \n| &&
        |12  r8   r8   r8                                                             \n| ).
  ENDMETHOD.

ENDCLASS.
