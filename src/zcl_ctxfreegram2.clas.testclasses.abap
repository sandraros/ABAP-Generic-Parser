*"* use this source file for your ABAP unit test classes

CLASS ltc_main DEFINITION DEFERRED.
CLASS ltc_action DEFINITION DEFERRED.
CLASS ltc_first_sets DEFINITION DEFERRED.
CLASS zcl_ctxfreegram2 DEFINITION LOCAL FRIENDS ltc_main ltc_action ltc_first_sets.
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
    METHODS item_sets_transitions FOR TESTING.
    METHODS circular_1 FOR TESTING.
    METHODS circular_2 FOR TESTING.
    METHODS test FOR TESTING.
    METHODS test2 FOR TESTING.
ENDCLASS.

CLASS ltc_main IMPLEMENTATION.

  METHOD missing_start_rule.

*    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
*    DATA(start) = factory->new_nonterminal( 'start' ).
*    TRY.
*        DATA(grammar) = NEW zcl_ctxfreegram2( start_rule = start rules = VALUE #( ) ).
*      CATCH zcx_ctxfreegram INTO DATA(lx).
*    ENDTRY.
*
*    IF lx IS NOT BOUND OR lx->error <> zcx_ctxfreegram=>c_error-missing_start_rule.
*      cl_abap_unit_assert=>fail( msg = 'Exception "start rule missing" expected' ).
*    ENDIF.

  ENDMETHOD.

  METHOD lhs_not_bound.

*    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
*    DATA(start) = factory->new_nonterminal( 'start' ).
*    TRY.
*        DATA(grammar) = NEW zcl_ctxfreegram2( start_rule = start rules = VALUE #( ( ) ) ).
*      CATCH zcx_ctxfreegram INTO DATA(lx).
*    ENDTRY.
*
*    IF lx IS NOT BOUND OR lx->error <> zcx_ctxfreegram=>c_error-lhs_element_not_bound.
*      cl_abap_unit_assert=>fail( msg = 'Exception "missing LHS" expected' ).
*    ENDIF.

  ENDMETHOD.

  METHOD start_rule_twice.

*    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
*    DATA(start) = factory->new_nonterminal( 'start' ).
*    TRY.
*        DATA(grammar) = NEW zcl_ctxfreegram2( start_rule = start rules = VALUE #(
*            ( lhs = start )
*            ( lhs = start ) ) ).
*      CATCH zcx_ctxfreegram INTO DATA(lx).
*    ENDTRY.
*
*    IF lx IS NOT BOUND OR lx->error <> zcx_ctxfreegram=>c_error-start_rule_must_appear_once.
*      cl_abap_unit_assert=>fail( msg = 'Exception "start rule must appear once" expected' ).
*    ENDIF.

  ENDMETHOD.

  METHOD unique_rule_one_character.

*    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
*    DATA(start) = factory->new_nonterminal( 'start' ).
*    DATA(letter_a) = factory->new_terminal( 'a' ).
*    DATA(grammar) = NEW zcl_ctxfreegram2( start_rule = start rules = VALUE #(
*        ( lhs = start rhs = letter_a ) ) ).
*
*    DATA(itemsets) = grammar->render_itemsets( ).
*    DATA(transitions) = grammar->render_transitions( ).
*    DATA(action_goto_tables) = grammar->render_action_goto_tables( ).
*
*    cl_abap_unit_assert=>assert_equals( act = itemsets exp =
*        |* Item set 1:\n| &
*        |** Item kernel: start = ◆ 'a'\n| &
*        |* Item set 2:\n| &
*        |** Item kernel: start = 'a' ◆\n| ).
*    cl_abap_unit_assert=>assert_equals( act = transitions exp =
*        |   '$'  'a'  start  \n| &
*        |1       2           \n| &
*        |2                   \n| ).
*    cl_abap_unit_assert=>assert_equals( act = action_goto_tables exp =
*        |   '$'  'a'  start  \n| &
*        |1       s2          \n| &
*        |2  acc              \n| ).

  ENDMETHOD.

  METHOD unique_rule_two_characters.

*    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
*    DATA(start) = factory->new_nonterminal( 'start' ).
*    DATA(letter_a) = factory->new_terminal( 'a' ).
*    DATA(grammar) = NEW zcl_ctxfreegram2( start_rule = start rules = VALUE #(
*        ( lhs = start rhs = factory->new_sequence( VALUE #( ( letter_a ) ( letter_a ) ) ) ) ) ).
*
*    DATA(itemsets) = grammar->render_itemsets( ).
*    DATA(transitions) = grammar->render_transitions( ).
*    DATA(action_goto_tables) = grammar->render_action_goto_tables( ).
*
*    cl_abap_unit_assert=>assert_equals( act = itemsets exp =
*        |* Item set 1:\n| &
*        |** Item kernel: start = ◆ 'a' 'a'\n| &
*        |* Item set 2:\n| &
*        |** Item kernel: start = 'a' ◆ 'a'\n| &
*        |* Item set 3:\n| &
*        |** Item kernel: start = 'a' 'a' ◆\n| ).
*    cl_abap_unit_assert=>assert_equals( act = transitions exp =
*        |   '$'  'a'  start  \n| &
*        |1       2           \n| &
*        |2       3           \n| &
*        |3                   \n| ).
*    cl_abap_unit_assert=>assert_equals( act = action_goto_tables exp =
*        |   '$'  'a'  start  \n| &
*        |1       s2          \n| &
*        |2       s3          \n| &
*        |3  acc              \n| ).

  ENDMETHOD.

  METHOD optional.

*    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
*    DATA(start) = factory->new_nonterminal( 'start' ).
*    DATA(optional) = factory->new_nonterminal( 'optional' ).
*    DATA(letter_a) = factory->new_terminal( 'a' ).
*    DATA(grammar) = NEW zcl_ctxfreegram2( start_rule = start rules = VALUE #(
*        ( lhs = start    rhs = optional )
*        ( lhs = optional rhs = letter_a )
*        ( lhs = optional ) ) ).
*
*    DATA(itemsets) = grammar->render_itemsets( ).
*    DATA(transitions) = grammar->render_transitions( ).
*    DATA(action_goto_tables) = grammar->render_action_goto_tables( ).
*
*    cl_abap_unit_assert=>assert_equals( act = itemsets exp =
*        |* Item set 1:\n| &
*        |** Item kernel: start = ◆ 'a' 'a'\n| &
*        |* Item set 2:\n| &
*        |** Item kernel: start = 'a' ◆ 'a'\n| &
*        |* Item set 3:\n| &
*        |** Item kernel: start = 'a' 'a' ◆\n| ).
*    cl_abap_unit_assert=>assert_equals( act = transitions exp =
*        |   '$'  'a'  start  \n| &
*        |1       2           \n| &
*        |2       3           \n| &
*        |3                   \n| ).
*    cl_abap_unit_assert=>assert_equals( act = action_goto_tables exp =
*        |   '$'  'a'  start  \n| &
*        |1       s2          \n| &
*        |2       s3          \n| &
*        |3  acc              \n| ).

  ENDMETHOD.

  METHOD recursive_rule.

*    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
*    DATA(start) = factory->new_nonterminal( 'start' ).
*    TRY.
*        DATA(grammar) = NEW zcl_ctxfreegram2( start_rule = start rules = VALUE #( ( lhs = start rhs = start ) ) ).
*      CATCH zcx_ctxfreegram INTO DATA(lx).
*    ENDTRY.
*
*    IF lx IS NOT BOUND OR lx->error <> zcx_ctxfreegram=>c_error-missing_start_rule.
*      cl_abap_unit_assert=>fail( msg = 'Exception "start rule missing" expected' ).
*    ENDIF.
*
**cl_abap_unit_assert=>assert_equals( act = act exp = exp msg = 'msg' ).

  ENDMETHOD.

  METHOD circular_1.

    DATA(grammar) = NEW zcl_ctxfreegram2( ).
    grammar->load_rules(
        rules = VALUE #(
            LET start = NEW lcl_non_terminal( 'start' ) IN
            ( NEW lcl_rule( lhs = start rhs = VALUE #( ( start ) ) ) ) ) ).

    TRY.
        grammar->create_first_sets( ).
        cl_abap_unit_assert=>fail( msg = 'should have failed' ).
      CATCH zcx_ctxfreegram INTO DATA(lx).
        cl_abap_unit_assert=>assert_equals( act = lx->error exp = lx->c_error-rules_circular_reference ).
    ENDTRY.

  ENDMETHOD.

  METHOD circular_2.

    DATA(grammar) = NEW zcl_ctxfreegram2( ).
    grammar->load_rules(
        rules = VALUE #(
            LET start = NEW lcl_non_terminal( 'start' )
                e     = NEW lcl_non_terminal( 'E' ) IN
            ( NEW lcl_rule( lhs = start rhs = VALUE #( ( e ) ) ) )
            ( NEW lcl_rule( lhs = e     rhs = VALUE #( ( start ) ) ) ) ) ).

    TRY.
        grammar->create_first_sets( ).
        cl_abap_unit_assert=>fail( msg = 'should have failed' ).
      CATCH zcx_ctxfreegram INTO DATA(lx).
        cl_abap_unit_assert=>assert_equals( act = lx->error exp = lx->c_error-rules_circular_reference ).
    ENDTRY.

  ENDMETHOD.

  METHOD item_sets_transitions.

*    DATA(factory) = NEW zcl_ctxfreegram_factory( ).

    DATA(grammar) = NEW zcl_ctxfreegram2( ).
    grammar->load_rules(
        rules = VALUE #(
            LET start = NEW lcl_non_terminal( 'start' )
                e     = NEW lcl_non_terminal( 'E' )
                b     = NEW lcl_non_terminal( 'B' )
                star  = NEW lcl_terminal( '*' )
                plus  = NEW lcl_terminal( '+' )
                _0    = NEW lcl_terminal( '0' )
                _1    = NEW lcl_terminal( '1' ) IN
            ( NEW lcl_rule( lhs = start rhs = VALUE #( ( e ) ) ) )
            ( NEW lcl_rule( lhs = e rhs = VALUE #( ( e ) ( star ) ( b ) ) ) )
            ( NEW lcl_rule( lhs = e rhs = VALUE #( ( e ) ( plus ) ( b ) ) ) )
            ( NEW lcl_rule( lhs = e rhs = VALUE #( ( b ) ) ) )
            ( NEW lcl_rule( lhs = b rhs = VALUE #( ( _0 ) ) ) )
            ( NEW lcl_rule( lhs = b rhs = VALUE #( ( _1 ) ) ) ) ) ).

    grammar->create_item_set_transit_tables( ).

    DATA(item_sets) = grammar->render_item_sets( ).
    cl_abap_unit_assert=>assert_equals( act = item_sets exp =
        |* Item set 1:\n| &
        |** Item kernel: start = ◆ E\n| &
        |** Item closure: E = ◆ E '*' B\n| &
        |** Item closure: E = ◆ E '+' B\n| &
        |** Item closure: E = ◆ B\n| &
        |** Item closure: B = ◆ '0'\n| &
        |** Item closure: B = ◆ '1'\n| &
        |* Item set 2:\n| &
        |** Item kernel: start = E ◆\n| &
        |** Item kernel: E = E ◆ '*' B\n| &
        |** Item kernel: E = E ◆ '+' B\n| &
        |* Item set 3:\n| &
        |** Item kernel: E = B ◆\n| &
        |* Item set 4:\n| &
        |** Item kernel: B = '0' ◆\n| &
        |* Item set 5:\n| &
        |** Item kernel: B = '1' ◆\n| &
        |* Item set 6:\n| &
        |** Item kernel: E = E '*' ◆ B\n| &
        |** Item closure: B = ◆ '0'\n| &
        |** Item closure: B = ◆ '1'\n| &
        |* Item set 7:\n| &
        |** Item kernel: E = E '+' ◆ B\n| &
        |** Item closure: B = ◆ '0'\n| &
        |** Item closure: B = ◆ '1'\n| &
        |* Item set 8:\n| &
        |** Item kernel: E = E '*' B ◆\n| &
        |* Item set 9:\n| &
        |** Item kernel: E = E '+' B ◆\n| ).

    DATA(transitions) = grammar->render_transitions( ).
    cl_abap_unit_assert=>assert_equals( act = transitions exp =
        |   '*'  '+'  '0'  '1'  E  B  \n| &
        |1            4    5    2  3  \n| &
        |2  6    7                    \n| &
        |3                            \n| &
        |4                            \n| &
        |5                            \n| &
        |6            4    5       8  \n| &
        |7            4    5       9  \n| &
        |8                            \n| &
        |9                            \n| ).

    grammar->create_first_sets( ).
    DATA(first_sets) = grammar->render_first_sets( ).
    cl_abap_unit_assert=>assert_equals( act = first_sets exp =
        |NON_TERMINAL  T_1  T_2  \n| &
        |B             '0'  '1'  \n| &
        |E             '0'  '1'  \n| &
        |start         '0'  '1'  \n| ).

    grammar->create_follow_sets( ).
    DATA(follow_sets) = grammar->render_follow_sets( ).
    cl_abap_unit_assert=>assert_equals( act = follow_sets exp =
        |SYMBOL  T_1    T_2  T_3  \n| &
        |start   'EOF'            \n| &
        |E       'EOF'  '*'  '+'  \n| &
        |B       'EOF'  '*'  '+'  \n| &
        |'0'     'EOF'  '*'  '+'  \n| &
        |'1'     'EOF'  '*'  '+'  \n| &
        |'*'     '0'    '1'       \n| &
        |'+'     '0'    '1'       \n| ).

  ENDMETHOD.

  METHOD test.

*    " start: E
*    " E: E '*' B
*    " E: E '+' B
*    " E: B
*    " B: '0'
*    " B: '1'
*    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
*    DATA(start) = factory->new_nonterminal( 'start' ).
*    DATA(e) = factory->new_nonterminal( 'E' ).
*    DATA(b) = factory->new_nonterminal( 'B' ).
*    DATA(star) = factory->new_terminal( '*' ).
*    DATA(plus) = factory->new_terminal( '+' ).
*    DATA(_0) = factory->new_terminal( '0' ).
*    DATA(_1) = factory->new_terminal( '1' ).
*
*    DATA(grammar) = NEW zcl_ctxfreegram2(
*        start_rule = start
*        rules = VALUE #(
*            ( lhs = start rhs = e )
*            ( lhs = e     rhs = factory->new_sequence( VALUE #( ( e ) ( star ) ( b ) ) ) )
*            ( lhs = e     rhs = factory->new_sequence( VALUE #( ( e ) ( plus ) ( b ) ) ) )
*            ( lhs = e     rhs = b )
*            ( lhs = b     rhs = _0 )
*            ( lhs = b     rhs = _1 ) ) ).
*
*    DATA(itemsets) = grammar->render_itemsets( ).
*    DATA(transitions) = grammar->render_transitions( ).
*    DATA(action_goto_tables) = grammar->render_action_goto_tables( ).
*
*    DATA(output) = grammar->render_rules( ) && |\n\n| && grammar->render_action_goto_tables( ).

  ENDMETHOD.

  METHOD test2.

*    " start: while
*    " while: 'while' condition
*    " condition: variable '=' number
*    " variable: 'regex:[a-zA-Z_][a-zA-Z_0-9]*'
*    " number: 'regex:[0-9][0-9]*'
*
*    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
*    DATA(start) = factory->new_nonterminal( 'start' ).
*    DATA(while) = factory->new_nonterminal( 'while' ).
*    DATA(condition) = factory->new_nonterminal( 'condition' ).
*    DATA(_while) = factory->new_terminal( 'while' ).
*    DATA(variable) = factory->new_nonterminal( 'variable' ).
*    DATA(variable_regex) = factory->new_terminal_regex( '[a-zA-Z_][a-zA-Z_0-9]*' ).
*    DATA(equal) = factory->new_terminal( '=' ).
*    DATA(number) = factory->new_nonterminal( 'number' ).
*    DATA(number_regex) = factory->new_terminal_regex( '[0-9][0-9]*' ).
*
*    DATA(grammar) = NEW zcl_ctxfreegram2(
*        start_rule = start
*        rules = VALUE #(
*            ( lhs = start     rhs = while )
*            ( lhs = while     rhs = factory->new_sequence( VALUE #( ( _while ) ( condition ) ) ) )
*            ( lhs = condition rhs = factory->new_sequence( VALUE #( ( variable ) ( equal ) ( number ) ) ) )
*            ( lhs = variable  rhs = variable_regex )
*            ( lhs = number    rhs = number_regex ) ) ).
*
*    DATA(itemsets) = grammar->render_itemsets( ).
*    DATA(transitions) = grammar->render_transitions( ).
*    DATA(action_goto_tables) = grammar->render_action_goto_tables( ).

  ENDMETHOD.

ENDCLASS.

CLASS ltc_first_sets DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    METHODS test FOR TESTING.
ENDCLASS.

CLASS ltc_first_sets IMPLEMENTATION.
  METHOD test.
*  data(grammar) = new zcl_ctxfreegram2( ).
**  grammar->dataprovider = new ltcl_dataprovider( ).
*data(transitions) = VALUE ty_xx(
*    ( ) ).
*data(item_sets) = VALUE ty_xx(
*    ( ) ).
*data(action_table) = grammar->create_action_table( item_sets = item_sets transitions = transitions ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_action DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    METHODS test FOR TESTING.
ENDCLASS.

CLASS ltc_action IMPLEMENTATION.
  METHOD test.
*  data(grammar) = new zcl_ctxfreegram2( ).
**  grammar->dataprovider = new ltcl_dataprovider( ).
*data(transitions) = VALUE ty_xx(
*    ( ) ).
*data(item_sets) = VALUE ty_xx(
*    ( ) ).
*data(action_table) = grammar->create_action_table( item_sets = item_sets transitions = transitions ).
*    " Part 1 of action determination = list of SHIFT
*    "   SIMPLEST WAY : VIA THE TRANSITION TABLE
*    "  (maybe this other solution :
*    "   - there's no SHIFT if dot is at the end of RHS
*    "   - If dot is not at the end of RHS:
*    "        - if symbol at dot is a terminal, it's this terminal
*    "        - if it's a nonterminal, there's a shift for all symbols of the First Set of this nonterminal except "$"
*    "  )
*    LOOP AT aut_transition ASSIGNING <ls_transition>
*          WHERE s_symbol-type = zcl_ctxfreegram=>lcs_symbol_type-terminal.
*      APPEND INITIAL LINE TO aut_action ASSIGNING <ls_action>.
*      <ls_action>-state           = <ls_transition>-itemset_index.
*      <ls_action>-terminal_index  = <ls_transition>-s_symbol-index.
*      <ls_action>-action          = lcs_action-shift.
*      <ls_action>-index           = <ls_transition>-goto_itemset_index.
*    ENDLOOP.
*
*    " Part 2 of action determination  = list of REDUCE
*    "   - if the dot is at the end of RHS
*    "        - there's no REDUCE
*    "   - else
*    "        - if symbol at dot is a terminal, it's REDUCE of this terminal
*    "        - if it's a nonterminal, take all symbols of the First Set of this nonterminal except "$"
*    LOOP AT aut_itemset ASSIGNING <ls_itemset>.
*      LOOP AT <ls_itemset>-t_item ASSIGNING <ls_item>
*            WHERE type = lcs_item_type-kernel
*              AND s_dot_symbol-type = zcl_ctxfreegram=>lcs_symbol_type-nonterminal.
*        LOOP AT aut_rule ASSIGNING <ls_rule>
*              WHERE lhs_nonterm_index = <ls_item>-s_dot_symbol-index
*                AND t_symbol IS INITIAL.
*          EXIT.
*        ENDLOOP.
*        IF sy-subrc = 0 AND 0 = lines( <ls_rule>-t_symbol ).
*          LOOP AT aut_follow_set ASSIGNING <ls_follow_set> WHERE symbol = <ls_item>-s_dot_symbol.
*            LOOP AT <ls_follow_set>-t_follow_symbol ASSIGNING <ls_symbol>.
*              APPEND INITIAL LINE TO aut_action ASSIGNING <ls_action>.
*              <ls_action>-state           = <ls_itemset>-index.
*              <ls_action>-terminal_index  = <ls_symbol>-index.
*              <ls_action>-action          = lcs_action-reduce.
*              <ls_action>-index           = <ls_rule>-index.
*            ENDLOOP.
*          ENDLOOP.
*        ENDIF.
*      ENDLOOP.
*
*      LOOP AT <ls_itemset>-t_item ASSIGNING <ls_item>
*            WHERE type = lcs_item_type-kernel
*              AND dot_at_the_end = abap_true.
*        READ TABLE aut_follow_set ASSIGNING <ls_follow_set>
*              WITH KEY  symbol-type  = zcl_ctxfreegram=>lcs_symbol_type-nonterminal
*                        symbol-index = <ls_item>-lhs_nonterm_index.
*        ASSERT sy-subrc = 0.
*        LOOP AT <ls_follow_set>-t_follow_symbol ASSIGNING <ls_symbol>.
*          APPEND INITIAL LINE TO aut_action ASSIGNING <ls_action>.
*          <ls_action>-state           = <ls_itemset>-index.
*          <ls_action>-terminal_index  = <ls_symbol>-index.
*          IF <ls_item>-rule_index = 1.
*            <ls_action>-action          = lcs_action-accept.
*            <ls_action>-index           = 0.
*          ELSE.
*            <ls_action>-action          = lcs_action-reduce.
*            <ls_action>-index           = <ls_item>-rule_index.
*          ENDIF.
*        ENDLOOP.
*      ENDLOOP.
*    ENDLOOP.
*
*    " if both SHIFT and reduce are possible, then keep SHIFT
*    LOOP AT aut_action ASSIGNING <ls_action> WHERE action = lcs_action-reduce.
*      READ TABLE aut_action WITH KEY  state           = <ls_action>-state
*                                      terminal_index  = <ls_action>-terminal_index
*                                      action          = lcs_action-shift
*                            TRANSPORTING NO FIELDS.
*      IF sy-subrc = 0.
*        DELETE aut_action.
*      ENDIF.
*    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
