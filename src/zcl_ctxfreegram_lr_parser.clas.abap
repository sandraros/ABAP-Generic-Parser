CLASS zcl_ctxfreegram_lr_parser DEFINITION
  PUBLIC
  CREATE PROTECTED.

  PUBLIC SECTION.

    TYPES ty_ast_line_type TYPE i.
    TYPES:
      BEGIN OF ty_ls_ast,
        from_token TYPE i,
        to_token   TYPE i,
        "! Reduced rule (or 0 if none)
        rule_id    TYPE zcl_ctxfreegram_grammar=>ty_us_rule-id,
        "! Number of elements of the RHS of the reduced rule
        elements   TYPE STANDARD TABLE OF sytabix WITH EMPTY KEY,
      END OF ty_ls_ast.
    TYPES ty_ut_ast TYPE STANDARD TABLE OF ty_ls_ast WITH EMPTY KEY.

    DATA trace_table  TYPE string_table READ-ONLY.
    DATA record_trace TYPE abap_bool    READ-ONLY.
    DATA ast          TYPE ty_ut_ast    READ-ONLY.
    DATA grammar      TYPE REF TO zcl_ctxfreegram_grammar READ-ONLY.
    DATA symbol_stack TYPE TABLE OF REF TO zif_ctxfreegram_parsed_symbol READ-ONLY.

    CLASS-METHODS create
      IMPORTING
        io_context_free_grammar TYPE REF TO zcl_ctxfreegram_grammar
        record_trace            TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)           TYPE REF TO zcl_ctxfreegram_lr_parser.

    METHODS get_ast_as_string_table
      IMPORTING
        with_rule_number TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)    TYPE string_table
      RAISING
        zcx_ctxfreegram.

    METHODS parse
      IMPORTING
        io_lexer TYPE REF TO zif_ctxfreegram_tokenizer OPTIONAL
      RAISING
        zcx_ctxfreegram_lr_parser
        zcx_ctxfreegram
        zcx_ctxfreegram_tokenizer.

protected section.
  PRIVATE SECTION.
    TYPES ty_state TYPE i.
    TYPES:
      BEGIN OF ty_ls_parse_stack,
        state      TYPE ty_state,
      END OF ty_ls_parse_stack.
    TYPES ty_lt_parse_stack TYPE STANDARD TABLE OF ty_ls_parse_stack WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_parse_step,
        state       TYPE ty_state,
        input       TYPE string,
        output      TYPE string,
        "! Stack of states (push after shift or goto, pop after reduce)
        stack       TYPE string,
        next_action TYPE string,
      END OF ty_parse_step.
    TYPES:
      BEGIN OF ts_token,
        object TYPE REF TO zif_ctxfreegram_token,
        index  TYPE i,
        text   TYPE string,
      END OF ts_token.

    CONSTANTS lcs_action LIKE zcl_ctxfreegram_grammar=>lcs_action VALUE zcl_ctxfreegram_grammar=>lcs_action.

    DATA parse_stack          TYPE ty_lt_parse_stack.
    DATA end_of_input         TYPE REF TO lcl_token_end_of_input.
    DATA current_token        TYPE ts_token. "REF TO zif_ctxfreegram_token.
    DATA auo_lexer            TYPE REF TO zif_ctxfreegram_tokenizer. "= record_trace

    METHODS arg
      IMPORTING
        arg         TYPE simple
      RETURNING
        VALUE(args) TYPE REF TO lcl_args.

    METHODS get_action
      IMPORTING
        i_state       TYPE ty_state
        i_token       TYPE ts_token
      RETURNING
        VALUE(result) TYPE REF TO zcl_ctxfreegram_grammar=>ty_us_action
      RAISING
        zcx_ctxfreegram_lr_parser.

    METHODS get_goto_state_for_reduce
      IMPORTING
        i_prior_state TYPE ty_state
        i_rule_index  TYPE zcl_ctxfreegram_grammar=>ty_us_rule-lhs_nonterm_index
      RETURNING
        VALUE(result) TYPE ty_state
      RAISING
        zcx_ctxfreegram_lr_parser.

    METHODS get_next_token
      IMPORTING
        i_state       TYPE i
      RETURNING
        VALUE(result) TYPE ts_token
      RAISING
        zcx_ctxfreegram_lr_parser
        zcx_ctxfreegram_tokenizer.

    METHODS get_rule_number
      IMPORTING
        i_rule_number         TYPE zcl_ctxfreegram_grammar=>ty_us_action-index
      RETURNING
        VALUE(r_reduced_rule) TYPE REF TO zcl_ctxfreegram_grammar=>ty_us_rule
      RAISING
        zcx_ctxfreegram_lr_parser.

    METHODS get_rule_text
      IMPORTING
        rule_index    TYPE i
      RETURNING
        VALUE(result) TYPE string.

    METHODS parse_stack_pop
      IMPORTING
        number_of_elements TYPE i
      RETURNING
        VALUE(result)      TYPE ty_lt_parse_stack.

    METHODS parse_stack_push
      IMPORTING
        state      TYPE ty_state.

    METHODS pop_push_symbols
      IMPORTING
        i_rule_number TYPE zcl_ctxfreegram_grammar=>ty_us_action-index
      RAISING
        zcx_ctxfreegram_lr_parser.

    METHODS sprintf
      IMPORTING
        format                TYPE csequence
        args                  TYPE REF TO lcl_args
      RETURNING
        VALUE(formatted_text) TYPE string.

    METHODS trace
      IMPORTING
        trace_text TYPE csequence.

    METHODS trace_if_active
      IMPORTING
        trace_text TYPE csequence.

ENDCLASS.



CLASS ZCL_CTXFREEGRAM_LR_PARSER IMPLEMENTATION.


  METHOD arg.
    CREATE OBJECT args.
    args->arg( arg ).
  ENDMETHOD.


  METHOD create.
    result = NEW zcl_ctxfreegram_lr_parser( ).
    result->grammar = io_context_free_grammar.
    result->record_trace = record_trace.
    result->end_of_input = lcl_token_end_of_input=>create( text = io_context_free_grammar->symbol_end_of_input-value ).
  ENDMETHOD.


  METHOD get_action.

    DATA(error_terminal_does_not_apply) = zcx_ctxfreegram_lr_parser=>c_error-parse_unexpected_token.
    LOOP AT grammar->aut_action REFERENCE INTO DATA(action)
         WHERE state = i_state.
      DATA(terminal) = REF #( grammar->aut_terminal[ action->terminal_index ] OPTIONAL ).
      IF sy-subrc <> 0.
        error_terminal_does_not_apply = zcx_ctxfreegram_lr_parser=>c_error-parse_bug_unexpected.
        EXIT.
      ENDIF.
      CASE TYPE OF terminal->object.
        WHEN TYPE zif_ctxfreegram_rule_term.
          IF CAST zif_ctxfreegram_rule_term( terminal->object )->value = i_token-text.
            error_terminal_does_not_apply = VALUE #( ).
            EXIT.
          ENDIF.
        WHEN TYPE zif_ctxfreegram_rule_termregex.
          IF CAST zif_ctxfreegram_rule_termregex( i_token-object )->applies( i_token-text ).
            error_terminal_does_not_apply = VALUE #( ).
            EXIT.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    IF error_terminal_does_not_apply IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_ctxfreegram_lr_parser
        EXPORTING
          error     = error_terminal_does_not_apply
          msgv1     = |{ i_token-object->get_detailed_info( ) }| "grammar->aut_terminal[ i_current_terminal_index ]-terminal }|
          msgv2     = |{ i_state }|
          lr_parser = me.
    ENDIF.

    IF record_trace = abap_true.
      trace( sprintf( format = '  getAction(state &1, lookahead &2) -> &3'
                      args   = arg( i_state )->arg( i_token-text )->arg(
                          SWITCH string( action->action
                                         WHEN lcs_action-shift THEN
                                           |shift to state { action->index }|
                                         WHEN lcs_action-reduce THEN
                                           |reduce rule { action->index } ({ get_rule_text( action->index ) })|
                                         WHEN lcs_action-accept THEN
                                           |accept|
                                         ELSE
                                           action->action ) ) ) ).
    ENDIF.
    result = action.
  ENDMETHOD.


  METHOD get_ast_as_string_table.

    TYPES:
        BEGIN OF ts_symbol_tree,
          level TYPE i,
          symbol type ref to zif_ctxfreegram_parsed_symbol,
        END OF ts_symbol_tree.
        TYPES tt_symbol_tree TYPE STANDARD TABLE OF ts_symbol_tree WITH EMPTY KEY.

    DATA(symbol_tree) = VALUE tt_symbol_tree( ( level  = 0
                                                symbol = symbol_stack[ 1 ] ) ).
    LOOP AT symbol_tree REFERENCE INTO DATA(node_of_symbol_tree).
      DATA(tabix) = sy-tabix.
      DATA(indent) = repeat( val = ` `
                             occ = 4 * node_of_symbol_tree->level ).
      CASE TYPE OF node_of_symbol_tree->symbol.
        WHEN TYPE zcl_ctxfreegram_parsed_term.
          DATA(parsed_term) = CAST zcl_ctxfreegram_parsed_term( node_of_symbol_tree->symbol ).
          INSERT |{ indent }Offset { parsed_term->token->offset }, length { parsed_term->token->length }: "{ parsed_term->token->get_text( ) }"|
                INTO TABLE result.
        WHEN TYPE zcl_ctxfreegram_parsed_nonterm.
          DATA(parsed_nonterm) = CAST zcl_ctxfreegram_parsed_nonterm( node_of_symbol_tree->symbol ).
          DATA(rule) = grammar->formatted_rules[ index = parsed_NONterm->rule_number ].
          INSERT |{ indent }{ rule-plain_text }{ COND #( WHEN with_rule_number = abap_true
                                                         THEN | (#{ parsed_NONterm->rule_number }) :| ) }|
                INTO TABLE result.
          DATA(tabix_2) = tabix.
          LOOP AT parsed_nonterm->child_symbols INTO DATA(parsed_symbol_2).
            tabix_2 = tabix_2 + 1.
            INSERT VALUE #( level  = node_of_symbol_tree->level + 1
                            symbol = parsed_symbol_2 )
                INTO symbol_tree
                INDEX tabix_2.
          ENDLOOP.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_goto_state_for_reduce.
    READ TABLE grammar->aut_goto
         WITH KEY state             = i_prior_state
                  lhs_nonterm_index = i_rule_index
         REFERENCE INTO DATA(goto).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ctxfreegram_lr_parser
        EXPORTING
          error     = zcx_ctxfreegram_lr_parser=>c_error-parse_bug_goto_not_found
          lr_parser = me.
    ENDIF.
    trace_if_active(
        |  goto( prior state { i_prior_state }, nonterminal { grammar->aut_nonterminal[ i_rule_index ]-nonterminal } ) ---> new state { goto->next_state }| ).

    result = goto->next_state.
  ENDMETHOD.


  METHOD get_next_token.

    IF auo_lexer->is_next_token( ).
      result-object = auo_lexer->get_next_token( ).
    ELSE.
      result-object = end_of_input.
    ENDIF.

    result-text = result-object->get_text( ).

    IF record_trace = abap_true.
      trace( |  lookahead() -> { result-object->get_detailed_info( ) }| ).
    ENDIF.

  ENDMETHOD.


  METHOD get_rule_number.

    r_reduced_rule  = REF #( grammar->formatted_rules[ i_rule_number ] OPTIONAL ).
    IF r_reduced_rule IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_ctxfreegram_lr_parser
        EXPORTING
          error     = zcx_ctxfreegram_lr_parser=>c_error-parse_bug_rule_not_found
          lr_parser = me.
    ENDIF.

  ENDMETHOD.


  METHOD get_rule_text.
    DATA(rule) = REF #( grammar->formatted_rules[ rule_index ] ).
    result = |{ rule->lhs->get_text( ) }: { COND #( WHEN rule->rhs IS BOUND THEN rule->rhs->get_text( ) ) }|.
  ENDMETHOD.


  METHOD parse.

    DATA state                    TYPE i.
    DATA l_current_terminal_index TYPE sytabix.

    auo_lexer = io_lexer.

    " read first token
    DATA(read_next_token) = abap_true.

    " define the first state
    state = grammar->au_starting_nonterminal.
    trace_if_active( |state = { state }| ).

    parse_stack = VALUE #( ( state = state ) ). "from_token = 1 to_token = 1 ) ).

    " Get the first token
    current_token = get_next_token( state ).

    DO.

      trace_if_active( |(iteration { sy-index })| ).

      " read action corresponding to current state + current token
      DATA(ls_action) = get_action( i_state = state
                                    i_token = current_token ).

      CASE ls_action->action.

        WHEN lcs_action-shift.
          "**********************
          " shift
          "**********************

          DATA(new_state) = ls_action->index.
          parse_stack_push( state = new_state ).

          " Push the last token on the stack
          INSERT zcl_ctxfreegram_parsed_term=>create( current_token-object )
              INTO TABLE symbol_stack.

          "--------------------
          " Push next state n onto the parse stack as the new current state.
          "--------------------
          state = new_state.

          current_token = get_next_token( state ).

        WHEN lcs_action-reduce.
          "**********************
          " reduce
          "**********************

          data(rule_number) = ls_action->index.

          DATA(reduced_rule) = get_rule_number( rule_number ).

          "---------------------------
          " Remove the matched topmost L symbols (and parse trees and associated state numbers) from the parse stack.
          " This exposes a prior state P that was expecting an instance of the LHS symbol.
          " NB: pop as many states as number of symbols in the production rule
          "    (a rule may have 0 symbol (epsilon production) -> do nothing)
          "---------------------------

          DATA(unstacked_elements) = parse_stack_pop( lines( reduced_rule->t_symbol ) ).

          "---------------------------
          " Join the L parse trees together as one parse tree with new root symbol LHS.
          " ??????????
          "---------------------------

          "--------------------
          " Lookup the next state n from row p and column LHS of the LHS Goto table.
          "--------------------
          state = get_goto_state_for_reduce( i_prior_state = parse_stack[ lines( parse_stack ) ]-state
                                             i_rule_index  = reduced_rule->lhs_nonterm_index ).

          "--------------------
          " Push the symbol and tree for LHS onto the parse stack.
          " Push next state n onto the parse stack as the new current state.
          "--------------------

          parse_stack_push( state = state ).

          "--------------------
          " Symbol stack
          "--------------------
          pop_push_symbols( rule_number ).


        WHEN lcs_action-accept.
          "--------------------
          " All correct!
          "--------------------
          pop_push_symbols( 1 ).
          trace_if_active( `  accept` ).
          EXIT.

        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_ctxfreegram_lr_parser
            EXPORTING
              error     = zcx_ctxfreegram_lr_parser=>c_error-parse_unexpected
              lr_parser = me.
      ENDCASE.

    ENDDO.

  ENDMETHOD.


  METHOD parse_stack_pop.

    DATA(from_tabix) = lines( parse_stack ) - number_of_elements + 1.
    DATA(to_tabix) = lines( parse_stack ).

    result = VALUE #( ( LINES OF parse_stack FROM from_tabix TO to_tabix ) ).

    DELETE parse_stack FROM from_tabix TO to_tabix.

  ENDMETHOD.


  METHOD parse_stack_push.

    INSERT VALUE ty_ls_parse_stack( state = state )
           INTO TABLE parse_stack.
    trace_if_active( |  pushParseStack( state { state } )| ).", action { is_action-index }, tokens { from_token }-{ to_token })| ).
  ENDMETHOD.


  METHOD pop_push_symbols.

    DATA(reduced_rule_2) = get_rule_number( i_rule_number ).
    DATA(child_symbols) = VALUE zcl_ctxfreegram_parsed_NONterm=>tt_parsed_symbol( ).
    LOOP AT symbol_stack INTO DATA(symbol)
          FROM ( lines( symbol_stack ) - lines( reduced_rule_2->t_symbol ) + 1 ).
      APPEND symbol TO child_symbols.
      DELETE symbol_stack USING KEY loop_key.
    ENDLOOP.
    " PUSH 1 SYMBOL
    INSERT zcl_ctxfreegram_parsed_NONterm=>create( rule_number   = i_rule_number
                                           child_symbols = child_symbols )
      INTO TABLE symbol_stack.

  ENDMETHOD.


  METHOD sprintf.
    DATA arg TYPE string.

    formatted_text = format.
    LOOP AT args->args INTO arg.
      REPLACE |&{ sy-tabix }| IN formatted_text WITH arg.
    ENDLOOP.
  ENDMETHOD.


  METHOD trace.
    INSERT trace_text INTO TABLE trace_table.
  ENDMETHOD.


  METHOD trace_if_active.
    IF record_trace = abap_true.
      trace( trace_text ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
