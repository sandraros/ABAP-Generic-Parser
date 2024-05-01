CLASS zcl_ctxfreegram_lr_parser DEFINITION
  PUBLIC
  CREATE PROTECTED.

  PUBLIC SECTION.

    TYPES ty_ast_line_type TYPE i.
    TYPES:
      BEGIN OF ty_ls_ast,
*        "! 1 = token, 2 = rule
*        line_type  TYPE ty_ast_line_type,
        from_token TYPE i,
        to_token   TYPE i,
        "! Reduced rule (or 0 if none)
        rule_id    TYPE zcl_ctxfreegram_grammar=>ty_us_rule-id,
        "! Number of elements of the RHS of the reduced rule
        elements   TYPE STANDARD TABLE OF sytabix WITH EMPTY KEY,
      END OF ty_ls_ast.
    TYPES ty_ut_ast TYPE STANDARD TABLE OF ty_ls_ast WITH EMPTY KEY.

*    CONSTANTS:
*      BEGIN OF ast_line_type,
*        token TYPE ty_ast_line_type VALUE 1,
*        rule  TYPE ty_ast_line_type VALUE 2,
*      END OF ast_line_type.

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
      RETURNING
        VALUE(result) TYPE string_table
      RAISING
        zcx_ctxfreegram.

    METHODS parse
      IMPORTING
*        text     TYPE string OPTIONAL
        io_lexer TYPE REF TO zif_ctxfreegram_tokenizer OPTIONAL
      RAISING
        zcx_ctxfreegram_lr_parser
        zcx_ctxfreegram
        zcx_ctxfreegram_tokenizer.

protected section.
  PRIVATE SECTION.
*    DATA symbol_stack TYPE TABLE OF REF TO ZIF_CTXFREEGRAM_PARSED_SYMBOL.
    TYPES ty_state TYPE i.
    TYPES:
      BEGIN OF ty_ls_parse_stack,
        state      TYPE ty_state,
*        "! s = shift, r = reduce, a = accept
*        "! (values in constant ZCL_CTXFREEGRAM_GRAMMAR=>LCS_ACTION)
*        action     TYPE zcl_ctxfreegram_grammar=>ty_u_action,
*        from_token TYPE i,
*        to_token   TYPE i,
*        "! only for reduce
*        ast_index  TYPE i,
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
*    DATA parse_steps          TYPE ty_parse_steps.
    DATA end_of_input         TYPE REF TO lcl_token_end_of_input.
*    DATA current_token_text   TYPE string.
    DATA current_token        TYPE ts_token. "REF TO zif_ctxfreegram_token.
*    DATA current_token_number TYPE i.
    DATA auo_lexer            TYPE REF TO zif_ctxfreegram_tokenizer. "= record_trace

    METHODS arg
      IMPORTING
        arg         TYPE simple
      RETURNING
        VALUE(args) TYPE REF TO lcl_args.

    METHODS append_ast
      IMPORTING
        is_action          TYPE zcl_ctxfreegram_grammar=>ty_us_action
        unstacked_elements TYPE ty_lt_parse_stack.
*        i_tabix       TYPE i.

    METHODS get_action
      IMPORTING
        i_state       TYPE ty_state
        i_token       TYPE ts_token
*        i_current_terminal_index TYPE sytabix
*        i_token_text             TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zcl_ctxfreegram_grammar=>ty_us_action
      RAISING
*        zcx_ctxfreegram
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
*        zcx_ctxfreegram
        zcx_ctxfreegram_lr_parser
        zcx_ctxfreegram_tokenizer.

    METHODS get_rule_number
      IMPORTING
        i_rule_number         TYPE zcl_ctxfreegram_grammar=>ty_us_action-index
      RETURNING
        VALUE(r_reduced_rule) TYPE REF TO zcl_ctxfreegram_grammar=>ty_us_rule
      RAISING
*        zcx_ctxfreegram
        zcx_ctxfreegram_lr_parser.

    METHODS get_rule_text
      IMPORTING
        rule_index    TYPE i
      RETURNING
        VALUE(result) TYPE string.

*    METHODS get_terminal_index_in_grammar
*      IMPORTING
*        i_token       TYPE ts_token
*      RETURNING
*        VALUE(result) TYPE sytabix
*      RAISING
*        zcx_ctxfreegram_lr_parser.

    METHODS parse_stack_pop
      IMPORTING
        number_of_elements TYPE i
      RETURNING
        VALUE(result)      TYPE ty_lt_parse_stack.

    METHODS parse_stack_push
      IMPORTING
        state      TYPE ty_state.
*        is_action  TYPE zcl_ctxfreegram_grammar=>ty_us_action
**        unstacked_elements TYPE ty_lt_parse_stack.
*        from_token TYPE i
*        to_token   TYPE i.
*        ast_index  TYPE i.

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

    METHODS pop_push_symbols
      IMPORTING
        i_rule_number TYPE zcl_ctxfreegram_grammar=>ty_us_action-index
      RAISING
*        zcx_ctxfreegram
        zcx_ctxfreegram_lr_parser.

ENDCLASS.



CLASS ZCL_CTXFREEGRAM_LR_PARSER IMPLEMENTATION.


  METHOD append_ast.
*    DATA(i2) = lines( ast ).
*    DATA(elements) = REDUCE #(
*                     INIT aux_elements = VALUE ty_ls_ast-elements( )
*                          i            = i2 + 1
*                     FOR <unstacked_element> IN unstacked_elements
*                     LET shift = xsdbool( <unstacked_element>-action = lcs_action-shift ) IN
*                     NEXT
*                          aux_elements = VALUE #(
*                              BASE aux_elements
*                              ( COND #( WHEN shift = abap_true THEN i ELSE <unstacked_element>-ast_index ) ) )
*                          i = COND #( WHEN shift = abap_true THEN i + 1 ELSE i ) ).
*
**          line_type  = ast_line_type-rule
*    APPEND VALUE #( rule_id    = is_action-index
*                    from_token = unstacked_elements[ 1 ]-from_token
*                    to_token   = unstacked_elements[ lines( unstacked_elements ) ]-to_token
*                    elements   = elements )
*        TO ast.
  ENDMETHOD.


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
*           AND terminal_index = i_token-index.
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
*          trace_if_active( sprintf( format = |  getRule( &1 ) ---> #symbols in rule = &2 , rule LHS = &3|
*                                    args   = arg( ls_action->index )->arg( lines( <ls_rule>-t_symbol ) )->arg(
*                                            grammar->aut_nonterminal[ <ls_rule>-lhs_nonterm_index ]-nonterminal ) ) ).
                                         WHEN lcs_action-accept THEN
                                           |accept|
                                         ELSE
                                           action->action ) ) ) ).
    ENDIF.
    result = action.
  ENDMETHOD.


  METHOD get_ast_as_string_table.

    TYPES:
      BEGIN OF ts_tree_symbol,
        level  TYPE i,
        symbol TYPE REF TO ZIF_CTXFREEGRAM_PARSED_SYMBOL,
      END OF ts_tree_symbol.
    TYPES tt_tree_symbol TYPE STANDARD TABLE OF ts_tree_symbol WITH EMPTY KEY.

    DATA(symbols) = VALUE tt_tree_symbol( ( level  = 0
                                            symbol = symbol_stack[ 1 ] ) ).
    LOOP AT symbols INTO DATA(symbol_1).
      DATA(current_symbol_tabix) = sy-tabix.
      DATA(indent) = repeat( val = ` `
                             occ = symbol_1-level * 4 ).
      CASE TYPE OF symbol_1-symbol.
        WHEN TYPE zcl_ctxfreegram_parsed_NONterm.
          DATA(nonterminal) = CAST zcl_ctxfreegram_parsed_NONterm( symbol_1-symbol ).
          DATA(rule) = REF #( grammar->formatted_rules[ nonterminal->rule_number ] ).
          INSERT |{ indent }{ rule->plain_text }| INTO TABLE result.
          DATA(insert_tabix) = current_symbol_tabix.
          LOOP AT nonterminal->child_symbols INTO DATA(child_symbol).
            insert_tabix = insert_tabix + 1.
            INSERT VALUE #( level  = symbol_1-level + 1
                            symbol = child_symbol )
                INTO symbols
                INDEX insert_tabix.
          ENDLOOP.
        WHEN TYPE zcl_ctxfreegram_parsed_term.
          DATA(terminal) = CAST zcl_ctxfreegram_parsed_term( symbol_1-symbol ).
          INSERT |{ indent }token: '{ terminal->token->get_text( ) }'| INTO TABLE result.
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
*    current_token_number = current_token_number + 1.

    IF record_trace = abap_true.
      trace( |  lookahead() -> { result-object->get_detailed_info( ) }| ).
    ENDIF.

*    result-index = get_terminal_index_in_grammar( result ).

  ENDMETHOD.


  METHOD get_rule_number.

    r_reduced_rule  = REF #( grammar->formatted_rules[ i_rule_number ] OPTIONAL ).
    IF r_reduced_rule IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_ctxfreegram_lr_parser
        EXPORTING
          error     = zcx_ctxfreegram_lr_parser=>c_error-parse_bug_rule_not_found
          lr_parser = me.
    ENDIF.
*    IF r_reduced_rule->t_symbol IS INITIAL.
*      RAISE EXCEPTION TYPE zcx_ctxfreegram
*        EXPORTING
*          error = zcx_ctxfreegram=>c_error-parse_bug_rule_without_element.
*    ENDIF.

*  ENDMETHOD.
*
*
*  METHOD get_terminal_index_in_grammar.
*
*    READ TABLE grammar->aut_terminal
*         WITH KEY terminal = i_token-text
*         REFERENCE INTO DATA(terminal).
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE zcx_ctxfreegram_lr_parser
*        EXPORTING
*          error     = zcx_ctxfreegram_lr_parser=>c_error-parse_token_not_in_grammar
*          msgv1     = CONV #( i_token-text )
*          lr_parser = me.
*    ENDIF.
*    result = terminal->index.

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
*    current_token_number = 0.

    " Get the first token
    current_token = get_next_token( state ).

    DO.

      trace_if_active( |(iteration { sy-index })| ).

      " read action corresponding to current state + current token
      DATA(ls_action) = get_action( i_state = state
                                    i_token = current_token ).
*                                    i_current_terminal_index = l_current_terminal_index
*                                    i_token_text             = current_token_text ).

*      parse_step = VALUE #( state  = state
*                            input  = |current token number: { current_token_number }; current character position: { length }|
**                            input  = |{ text(length) } ◆ { text+length }|
*                            output = concat_lines_of(
*                                         sep   = ','
*                                         table = VALUE string_table( FOR <aux_reduced_rule> IN reduced_rules
*                                                                     ( |{ <aux_reduced_rule> }| ) ) )
*                            stack  = concat_lines_of(
*                                         sep   = ','
*                                         table = VALUE string_table( FOR <aux_parse_stack> IN parse_stack
*                                                                     ( |{ <aux_parse_stack>-state }| ) ) ) ).

      CASE ls_action->action.

        WHEN lcs_action-shift.
          "**********************
          " shift
          "**********************

          DATA(new_state) = ls_action->index.
          parse_stack_push( state = new_state ).
*                            is_action  = ls_action->*
*                            from_token = current_token_number
*                            to_token   = current_token_number ).

          " Push the last token on the stack
          APPEND zcl_ctxfreegram_parsed_term=>create( current_token-object ) TO symbol_stack.

*          "--------------------
*          " Shift the matched terminal t onto the parse stack and scan the next input symbol into the lookahead buffer.
*          "--------------------
*
*          length = length + current_token->length.

          "--------------------
          " Push next state n onto the parse stack as the new current state.
          "--------------------
          state = new_state.

*          parse_step-next_action = |{ ls_action->action }{ ls_action->index }|.
*          APPEND parse_step TO parse_steps.

          current_token = get_next_token( state ).

*          trace_if_active( |  next state { next_state }| ).
*          trace_if_active( |  pushParseStack( token "{ token_text }" )| ).

        WHEN lcs_action-reduce.
          "**********************
          " reduce
          "**********************

          data(rule_number) = ls_action->index.

*          APPEND rule_number TO reduced_rules.

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
*          append_ast( is_action          = ls_action->*
*                      unstacked_elements = unstacked_elements ).

          parse_stack_push( state = state ).
*                            is_action = ls_action->*
*                            unstacked_elements = unstacked_elements ).
*                            from_token = unstacked_elements[ 1 ]-from_token
*                            to_token   = unstacked_elements[ lines( unstacked_elements ) ]-to_token ).
*                            ast_index  = lines( ast ) ).

          "--------------------
          " Symbol stack
          "--------------------
          pop_push_symbols( rule_number ).

*          parse_step-next_action = |{ ls_action->action }{ ls_action->index }|.
*          APPEND parse_step TO parse_steps.

        WHEN lcs_action-accept.
          "--------------------
          " All correct!
          "--------------------
          pop_push_symbols( 1 ).
          trace_if_active( `  accept` ).
*          parse_step-next_action = `accept`.
*          APPEND parse_step TO parse_steps.
          EXIT.

        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_ctxfreegram_lr_parser
            EXPORTING
              error     = zcx_ctxfreegram_lr_parser=>c_error-parse_unexpected
              lr_parser = me.
      ENDCASE.

*      parse_step-next_action = |{ ls_action->action }{ ls_action->index }|.
*      APPEND parse_step TO parse_steps.
*
*      state = next_state.
*
*      IF ls_action->action = lcs_action-accept.
*        EXIT.
*      ENDIF.

    ENDDO.

    get_ast_as_string_table( ).

  ENDMETHOD.


  METHOD parse_stack_pop.

    DATA(from_tabix) = lines( parse_stack ) - number_of_elements + 1.
    DATA(to_tabix) = lines( parse_stack ).

    result = VALUE #( ( LINES OF parse_stack FROM from_tabix TO to_tabix ) ).

    DELETE parse_stack FROM from_tabix TO to_tabix.

  ENDMETHOD.


  METHOD parse_stack_push.

    APPEND VALUE ty_ls_parse_stack( state = state )
*                                    action     = is_action-index
*                                    from_token = from_token
*                                    to_token   = to_token )
           TO parse_stack.
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
    APPEND zcl_ctxfreegram_parsed_NONterm=>create( rule_number   = i_rule_number
                                           child_symbols = child_symbols )
      TO symbol_stack.

  ENDMETHOD.


  METHOD sprintf.
    DATA arg TYPE string.

    formatted_text = format.
    LOOP AT args->args INTO arg.
      REPLACE |&{ sy-tabix }| IN formatted_text WITH arg.
    ENDLOOP.
  ENDMETHOD.


  METHOD trace.
    APPEND trace_text TO trace_table.
  ENDMETHOD.


  METHOD trace_if_active.
    IF record_trace = abap_true.
      trace( trace_text ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
