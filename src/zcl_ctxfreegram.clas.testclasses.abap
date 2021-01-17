*"* use this source file for your ABAP unit test classes

CLASS ltc_main DEFINITION DEFERRED.
CLASS zcl_ctxfreegram DEFINITION LOCAL FRIENDS ltc_main.
*CLASS lcl_non_terminal DEFINITION FRIENDS ltc_main.

CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS missing_start_rule FOR TESTING.
    METHODS lhs_not_bound FOR TESTING.
    METHODS start_rule_twice FOR TESTING.
    METHODS unique_rule_one_character FOR TESTING.
    METHODS unique_rule_two_characters FOR TESTING.
    METHODS optional FOR TESTING.
    METHODS recursive_rule FOR TESTING.
    METHODS test FOR TESTING.
    METHODS test2 FOR TESTING.
ENDCLASS.

CLASS ltc_main IMPLEMENTATION.

  METHOD missing_start_rule.

    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    TRY.
        DATA(grammar) = NEW zcl_ctxfreegram( start_rule = start rules = VALUE #( ) ).
      CATCH zcx_ctxfreegram INTO DATA(lx).
    ENDTRY.

    IF lx IS NOT BOUND OR lx->error <> zcx_ctxfreegram=>c_error-missing_start_rule.
      cl_abap_unit_assert=>fail( msg = 'Exception "start rule missing" expected' ).
    ENDIF.

  ENDMETHOD.

  METHOD lhs_not_bound.

    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    TRY.
        DATA(grammar) = NEW zcl_ctxfreegram( start_rule = start rules = VALUE #( ( ) ) ).
      CATCH zcx_ctxfreegram INTO DATA(lx).
    ENDTRY.

    IF lx IS NOT BOUND OR lx->error <> zcx_ctxfreegram=>c_error-lhs_element_not_bound.
      cl_abap_unit_assert=>fail( msg = 'Exception "missing LHS" expected' ).
    ENDIF.

  ENDMETHOD.

  METHOD start_rule_twice.

    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    TRY.
        DATA(grammar) = NEW zcl_ctxfreegram( start_rule = start rules = VALUE #(
            ( lhs = start )
            ( lhs = start ) ) ).
      CATCH zcx_ctxfreegram INTO DATA(lx).
    ENDTRY.

    IF lx IS NOT BOUND OR lx->error <> zcx_ctxfreegram=>c_error-start_rule_must_appear_once.
      cl_abap_unit_assert=>fail( msg = 'Exception "start rule must appear once" expected' ).
    ENDIF.

  ENDMETHOD.

  METHOD unique_rule_one_character.

    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    DATA(letter_a) = factory->new_terminal( 'a' ).
    DATA(grammar) = NEW zcl_ctxfreegram( start_rule = start rules = VALUE #(
        ( lhs = start rhs = letter_a ) ) ).

    DATA(itemsets) = grammar->render_itemsets( ).
    DATA(transitions) = grammar->render_transitions( ).
    DATA(action_goto_tables) = grammar->render_action_goto_tables( ).

    cl_abap_unit_assert=>assert_equals( act = itemsets exp =
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

    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    DATA(letter_a) = factory->new_terminal( 'a' ).
    DATA(grammar) = NEW zcl_ctxfreegram( start_rule = start rules = VALUE #(
        ( lhs = start rhs = factory->new_sequence( VALUE #( ( letter_a ) ( letter_a ) ) ) ) ) ).

    DATA(itemsets) = grammar->render_itemsets( ).
    DATA(transitions) = grammar->render_transitions( ).
    DATA(action_goto_tables) = grammar->render_action_goto_tables( ).

    cl_abap_unit_assert=>assert_equals( act = itemsets exp =
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

  METHOD optional.

    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    DATA(optional) = factory->new_nonterminal( 'optional' ).
    DATA(letter_a) = factory->new_terminal( 'a' ).
    DATA(grammar) = NEW zcl_ctxfreegram( start_rule = start rules = VALUE #(
        ( lhs = start    rhs = optional )
        ( lhs = optional rhs = letter_a )
        ( lhs = optional ) ) ).

    DATA(itemsets) = grammar->render_itemsets( ).
    DATA(transitions) = grammar->render_transitions( ).
    DATA(action_goto_tables) = grammar->render_action_goto_tables( ).

    cl_abap_unit_assert=>assert_equals( act = itemsets exp =
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

  METHOD recursive_rule.

    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    TRY.
        DATA(grammar) = NEW zcl_ctxfreegram( start_rule = start rules = VALUE #( ( lhs = start rhs = start ) ) ).
      CATCH zcx_ctxfreegram INTO DATA(lx).
    ENDTRY.

    IF lx IS NOT BOUND OR lx->error <> zcx_ctxfreegram=>c_error-missing_start_rule.
      cl_abap_unit_assert=>fail( msg = 'Exception "start rule missing" expected' ).
    ENDIF.

*cl_abap_unit_assert=>assert_equals( act = act exp = exp msg = 'msg' ).

  ENDMETHOD.

  METHOD test.

    " start: E
    " E: E '*' B
    " E: E '+' B
    " E: B
    " B: '0'
    " B: '1'
    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    DATA(e) = factory->new_nonterminal( 'E' ).
    DATA(b) = factory->new_nonterminal( 'B' ).
    DATA(star) = factory->new_terminal( '*' ).
    DATA(plus) = factory->new_terminal( '+' ).
    DATA(_0) = factory->new_terminal( '0' ).
    DATA(_1) = factory->new_terminal( '1' ).

    DATA(grammar) = NEW zcl_ctxfreegram(
        start_rule = start
        rules = VALUE #(
            ( lhs = start rhs = e )
            ( lhs = e     rhs = factory->new_sequence( VALUE #( ( e ) ( star ) ( b ) ) ) )
            ( lhs = e     rhs = factory->new_sequence( VALUE #( ( e ) ( plus ) ( b ) ) ) )
            ( lhs = e     rhs = b )
            ( lhs = b     rhs = _0 )
            ( lhs = b     rhs = _1 ) ) ).

    DATA(itemsets) = grammar->render_itemsets( ).
    DATA(transitions) = grammar->render_transitions( ).
    DATA(action_goto_tables) = grammar->render_action_goto_tables( ).

    DATA(output) = grammar->render_rules( ) && |\n\n| && grammar->render_action_goto_tables( ).

  ENDMETHOD.

  METHOD test2.

    " start: while
    " while: 'while' condition
    " condition: variable '=' number
    " variable: 'regex:[a-zA-Z_][a-zA-Z_0-9]*'
    " number: 'regex:[0-9][0-9]*'

    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    DATA(while) = factory->new_nonterminal( 'while' ).
    DATA(condition) = factory->new_nonterminal( 'condition' ).
    DATA(_while) = factory->new_terminal( 'while' ).
    DATA(variable) = factory->new_nonterminal( 'variable' ).
    DATA(variable_regex) = factory->new_terminal_regex( '[a-zA-Z_][a-zA-Z_0-9]*' ).
    DATA(equal) = factory->new_terminal( '=' ).
    DATA(number) = factory->new_nonterminal( 'number' ).
    DATA(number_regex) = factory->new_terminal_regex( '[0-9][0-9]*' ).

    DATA(grammar) = NEW zcl_ctxfreegram(
        start_rule = start
        rules = VALUE #(
            ( lhs = start     rhs = while )
            ( lhs = while     rhs = factory->new_sequence( VALUE #( ( _while ) ( condition ) ) ) )
            ( lhs = condition rhs = factory->new_sequence( VALUE #( ( variable ) ( equal ) ( number ) ) ) )
            ( lhs = variable  rhs = variable_regex )
            ( lhs = number    rhs = number_regex ) ) ).

    DATA(itemsets) = grammar->render_itemsets( ).
    DATA(transitions) = grammar->render_transitions( ).
    DATA(action_goto_tables) = grammar->render_action_goto_tables( ).

  ENDMETHOD.

ENDCLASS.
