CLASS zcl_ctxfreegram_lr_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES : ty_ast_line_type TYPE i,
            BEGIN OF ty_ls_ast,
              line_type  TYPE ty_ast_line_type,
              from_token TYPE i,
              to_token   TYPE i,
              rule_id    TYPE zcl_ctxfreegram=>ty_us_rule-id,
              elements   TYPE STANDARD TABLE OF sytabix WITH EMPTY KEY,
            END OF ty_ls_ast,
            ty_ut_ast TYPE STANDARD TABLE OF ty_ls_ast WITH EMPTY KEY.
    CONSTANTS: BEGIN OF ast_line_type,
                 token TYPE ty_ast_line_type VALUE 1,
                 rule  TYPE ty_ast_line_type VALUE 2,
               END OF ast_line_type.
*    DATA auo_lexer      TYPE REF TO zif_ctxfreegram_lexer READ-ONLY.
*    DATA auo_agent      TYPE REF TO lif_parser_run_agent READ-ONLY.
    DATA trace_table TYPE string_table READ-ONLY.
    DATA record_trace TYPE abap_bool READ-ONLY.
    DATA ast TYPE ty_ut_ast READ-ONLY.
    METHODS constructor
      IMPORTING
        io_context_free_grammar TYPE REF TO zcl_ctxfreegram
*            io_agent      TYPE REF TO lif_parser_run_agent
        record_trace            TYPE abap_bool DEFAULT abap_false.
    METHODS trace IMPORTING trace_text TYPE csequence.

    METHODS parse
      IMPORTING
        text     TYPE string
        io_lexer TYPE REF TO zif_ctxfreegram_lexer OPTIONAL
      RAISING
        zcx_ctxfreegram.

    DATA grammar TYPE REF TO zcl_ctxfreegram READ-ONLY.

  PRIVATE SECTION.
    METHODS sprintf IMPORTING format TYPE csequence args TYPE REF TO lcl_args RETURNING VALUE(formatted_text) TYPE string.
    METHODS arg IMPORTING arg TYPE simple RETURNING VALUE(args) TYPE REF TO lcl_args.
    TYPES : BEGIN OF ty_ls_parse_stack,
              state      TYPE i,
              action     TYPE zcl_ctxfreegram=>ty_u_action,
              from_token TYPE i,
              to_token   TYPE i,
              ast_index  TYPE i, " only for reduce
            END OF ty_ls_parse_stack,
            ty_lt_parse_stack TYPE STANDARD TABLE OF ty_ls_parse_stack WITH EMPTY KEY.
    METHODS trace_pop_parse_stack
      IMPORTING
        i_tabix       TYPE sytabix
        i_parse_stack TYPE ty_lt_parse_stack.
    METHODS build_ast
      IMPORTING
        is_action type zcl_ctxfreegram=>ty_us_action
        i_parse_stack TYPE zcl_ctxfreegram_lr_parser=>ty_lt_parse_stack
        i_tabix       TYPE i.

    CONSTANTS lcs_action LIKE zcl_ctxfreegram=>lcs_action VALUE zcl_ctxfreegram=>lcs_action.

ENDCLASS.



CLASS zcl_ctxfreegram_lr_parser IMPLEMENTATION.

  METHOD constructor.
    grammar = io_context_free_grammar.
*    auo_agent = io_agent.
    me->record_trace = record_trace .
  ENDMETHOD.

  METHOD parse.
    TYPES: BEGIN OF ty_parse_step,
             state       TYPE i,
             input       TYPE string,
             output      TYPE string,
             stack       TYPE string,
             next_action TYPE string,
           END OF ty_parse_step,
           ty_parse_steps TYPE STANDARD TABLE OF ty_parse_step WITH EMPTY KEY.
    DATA parse_step TYPE ty_parse_step.
    DATA parse_steps TYPE ty_parse_steps.
    DATA reduced_rules TYPE TABLE OF i.
    DATA: state                    TYPE i,
          prior_state              TYPE i,
          next_state               TYPE i,
*          l_token                   TYPE lif_lexer=>ty_u_token,
          object                   TYPE REF TO object,
          l_current_terminal_index TYPE sytabix,
          l_tabix                  TYPE sytabix,
*          token_pos                TYPE sytabix,
          parse_stack              TYPE ty_lt_parse_stack,
          ls_parse_stack           TYPE ty_ls_parse_stack,
          ls_ast                   TYPE ty_ls_ast,
          ls_ast_reduce            TYPE ty_ls_ast.
    DATA auo_lexer      TYPE REF TO zif_ctxfreegram_lexer.
    FIELD-SYMBOLS:
      <ls_terminal>     TYPE zcl_ctxfreegram=>ty_us_terminal,
      <ls_nonterminal>  TYPE zcl_ctxfreegram=>ty_us_nonterminal,
      <ls_action>       TYPE zcl_ctxfreegram=>ty_us_action,
      <ls_goto>         TYPE zcl_ctxfreegram=>ty_us_goto,
      <ls_itemset>      TYPE zcl_ctxfreegram=>ty_us_itemset,
      <ls_rule>         TYPE zcl_ctxfreegram=>ty_us_rule,
      <l_current_state> TYPE sytabix,
      <ls_parse_stack>  TYPE ty_ls_parse_stack.

    DATA(length) = 0.

    IF io_lexer IS BOUND.
      auo_lexer = io_lexer.
    ELSE.
      auo_lexer = NEW lcl_default_lexer( text ).
    ENDIF.

    DATA(end_of_file) = abap_false.

    " read first token
    DATA(read_next_token) = abap_true.

    " define the first state
    state = grammar->au_starting_nonterminal.
    IF record_trace = abap_true.
      trace( |state = { state }| ).
    ENDIF.

    parse_stack = VALUE #( )."( state = state from_token = 1 to_token = 1 ) ).
    data(token_pos) = 0.

    DO.

      IF record_trace = abap_true.
        trace( |(iteration { sy-index })| ).
      ENDIF.

      " SHIFT
      IF read_next_token = abap_true.
        TRY.
            DATA(token) = auo_lexer->get_next_token( ).
          CATCH zcx_ctxfreegram.
            l_current_terminal_index = grammar->au_terminal_end_of_input.
            token = VALUE #( ).
        ENDTRY.
        IF l_current_terminal_index <> grammar->au_terminal_end_of_input.
          ADD 1 TO token_pos.
          DATA(token2) = substring( val = text off = token-offset len = token-length ).
          IF record_trace = abap_true.
            trace( |  lookahead() -> { token2 }| ).
          ENDIF.
          APPEND VALUE ty_ls_ast(
                line_type  = ast_line_type-token
                from_token = token_pos
                to_token   = token_pos )
                TO ast.
          APPEND VALUE ty_ls_parse_stack(
                    state      = state
                    action     = lcs_action-shift
                    from_token = token_pos
                    to_token   = token_pos
                    ast_index  = lines( ast ) )
                TO parse_stack.
          READ TABLE grammar->aut_terminal
                WITH KEY terminal = token2
                ASSIGNING <ls_terminal>.
          IF sy-subrc <> 0.
            " TODO search among REGEX terminals
            RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING error = zcx_ctxfreegram=>c_error-parse_unexpected_token.
          ELSE.
            l_current_terminal_index = <ls_terminal>-index.
          ENDIF.
        ENDIF.
        read_next_token = abap_false.
      ENDIF.

      " read action corresponding to current state + current token
      READ TABLE grammar->aut_action
            WITH KEY
              state           = state
              terminal_index  = l_current_terminal_index
            ASSIGNING <ls_action>.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING error = zcx_ctxfreegram=>c_error-parse_unexpected_token.
      ENDIF.
      IF record_trace = abap_true.
        trace( sprintf( format = '  getAction(state &1, lookahead &2) -> &3'
                        args   = arg( state )->arg( token2 )->arg( SWITCH string( <ls_action>-action
                                                                        WHEN lcs_action-shift THEN |shift to state { <ls_action>-index }|
                                                                        WHEN lcs_action-reduce THEN |reduce rule { <ls_action>-index }|
                                                                        WHEN lcs_action-accept THEN |accept|
                                                                        ELSE <ls_action>-action ) ) ) ).
      ENDIF.

      parse_step = VALUE #(
        state  = state
        input  = |{ text(length) } ◆ { text+length }|
        output = concat_lines_of( sep = ',' table = VALUE string_table( FOR <aux_reduced_rule> IN reduced_rules ( |{ <aux_reduced_rule> }| ) ) )
        stack  = concat_lines_of( sep = ',' table = VALUE string_table( FOR <aux_parse_stack> IN parse_stack ( |{ <aux_parse_stack>-state }| ) ) ) ).


      CASE <ls_action>-action.

        WHEN lcs_action-shift.
          "**********************
          " shift
          "**********************

          "--------------------
          " Shift the matched terminal t onto the parse stack and scan the next input symbol into the lookahead buffer.
          "--------------------

          read_next_token = abap_true.
          length = length + token-length.

          "--------------------
          " Push next state n onto the parse stack as the new current state.
          "--------------------
          next_state = <ls_action>-index.
          IF record_trace = abap_true.
            trace( |  next state { next_state }| ).
          ENDIF.
          IF record_trace = abap_true.
            trace( |  pushParseTree( token { token2 } )| ).
          ENDIF.


        WHEN lcs_action-reduce.
          "**********************
          " reduce
          "**********************

          APPEND <ls_action>-index TO reduced_rules.

          ASSIGN grammar->aut_rule[ <ls_action>-index ] TO <ls_rule>.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING error = zcx_ctxfreegram=>c_error-parse_bug_rule_not_found.
          ENDIF.
          IF <ls_rule>-t_symbol IS INITIAL.
            RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING error = zcx_ctxfreegram=>c_error-parse_bug_rule_without_element.
          ENDIF.

          IF record_trace = abap_true.
            trace( sprintf( format = |  getRule( &1 ) ---> #tokens &2 , nonterminal &3|
                            args   = arg( <ls_action>-index )->arg( lines( <ls_rule>-t_symbol ) )->arg( grammar->aut_nonterminal[ <ls_rule>-lhs_nonterm_index ]-nonterminal ) ) ).
          ENDIF.

          "---------------------------
          " Remove the matched topmost L symbols (and parse trees and associated state numbers) from the parse stack.
          " This exposes a prior state P that was expecting an instance of the LHS symbol.
          " NB: pop as many states as number of symbols in the production rule
          "    (a rule may have 0 symbol (epsilon production) -> do nothing)
          "---------------------------

          l_tabix = lines( parse_stack ) - lines( <ls_rule>-t_symbol ) + 1.
          build_ast( is_action = <ls_action> i_parse_stack = parse_stack i_tabix = l_tabix ).
          IF record_trace = abap_true.
            trace_pop_parse_stack( i_parse_stack = parse_stack i_tabix = l_tabix ).
          ENDIF.
          DELETE parse_stack FROM l_tabix.
          prior_state = parse_stack[ lines( parse_stack ) ]-state.

          "---------------------------
          " Join the L parse trees together as one parse tree with new root symbol LHS.
          " ??????????
          "---------------------------

          "--------------------
          " Lookup the next state n from row p and column LHS of the LHS Goto table.
          "--------------------
          READ TABLE grammar->aut_goto
                WITH KEY
                  state             = prior_state
                  lhs_nonterm_index = <ls_rule>-lhs_nonterm_index
                ASSIGNING <ls_goto>.
          IF sy-subrc <> 0.
             RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING error = zcx_ctxfreegram=>c_error-parse_bug_goto_not_found.
          ENDIF.
          next_state = <ls_goto>-next_state.
          IF record_trace = abap_true.
            trace( |  goto( prior state { prior_state }, nonterminal { grammar->aut_nonterminal[ <ls_rule>-lhs_nonterm_index ]-nonterminal } ) ---> new state { next_state }| ).
          ENDIF.

          "--------------------
          " Push the symbol and tree for LHS onto the parse stack.
          " Push next state n onto the parse stack as the new current state.
          "--------------------
          APPEND VALUE ty_ls_parse_stack(
                    state      = next_state
                    action     = lcs_action-reduce
                    from_token = parse_stack[ lines( parse_stack ) ]-from_token
                    to_token   = parse_stack[ lines( parse_stack ) ]-to_token
                    ast_index  = lines( ast ) )
                TO parse_stack.
          IF record_trace = abap_true.
            trace( |  pushParseTree( reduce rule { <ls_action>-index } )| ).
          ENDIF.


        WHEN lcs_action-accept.
          "--------------------
          " All correct!
          "--------------------
          IF record_trace = abap_true.
            trace( `  accept` ).
          ENDIF.
      ENDCASE.

      parse_step-next_action = |{ <ls_action>-action }{ <ls_action>-index }|.
      APPEND parse_step TO parse_steps.

      state = next_state.

      IF <ls_action>-action = lcs_action-accept.
        EXIT.
      ENDIF.

    ENDDO.

  ENDMETHOD.

  METHOD sprintf.
    DATA arg TYPE string.
    formatted_text = format.
    LOOP AT args->args INTO arg.
      REPLACE |&{ sy-tabix }| IN formatted_text WITH arg.
    ENDLOOP.
  ENDMETHOD.

  METHOD arg.
    CREATE OBJECT args.
    args->arg( arg ).
  ENDMETHOD.

  METHOD trace.
    APPEND trace_text TO trace_table.
  ENDMETHOD.

  METHOD trace_pop_parse_stack.
    DATA(trace_addendum) = |, prior state { i_parse_stack[ i_tabix ]-state }|.
    LOOP AT i_parse_stack FROM i_tabix REFERENCE INTO DATA(ls_parse_stack).
      CASE ls_parse_stack->action.
        WHEN lcs_action-shift.
          trace( |  popParseTree( ) -> token #{ ls_parse_stack->from_token }{ trace_addendum }| ).
          trace_addendum = ``.
        WHEN lcs_action-reduce.
          "TODO trace( |popParseTree( ) -> rule { ast[ ls_parse_stack->ast_index ]-rule_id }{ trace_addendum }| ).
          trace_addendum = ``.
      ENDCASE.
    ENDLOOP.
    LOOP AT i_parse_stack FROM i_tabix REFERENCE INTO ls_parse_stack.
      trace( |  pushAST( token #{ ls_parse_stack->from_token } )| ).
    ENDLOOP.
  ENDMETHOD.


  METHOD build_ast.

    APPEND LINES OF VALUE ty_ut_ast(
              LET i2 = lines( ast ) IN
*              ( LINES OF VALUE #(
*                  FOR <aux_parse_stack> IN i_parse_stack FROM i_tabix
*                  ( line_type  = ast_line_type-token
*                    from_token = <aux_parse_stack>-from_token
*                    to_token   = <aux_parse_stack>-to_token ) ) )
              ( line_type  = ast_line_type-rule
                rule_id    = is_action-index
                from_token = i_parse_stack[ i_tabix - 1 ]-from_token
                to_token   = i_parse_stack[ lines( i_parse_stack ) ]-to_token
                elements   = REDUCE #(
                              INIT e = VALUE ty_ls_ast-elements( )
                                   i = i2 + 1
                              FOR <aux_parse_stack> IN i_parse_stack FROM i_tabix
                              LET shift = xsdbool( <aux_parse_stack>-action = lcs_action-shift ) IN
                              NEXT e = VALUE #( BASE e ( COND #( WHEN shift = abap_true THEN i ELSE <aux_parse_stack>-ast_index ) ) )
                                   i = COND #( WHEN shift = abap_true THEN i + 1 ELSE i ) ) ) )
          TO ast.
*    IF record_trace = abap_true.
*      trace( |pushAST( rule { <ls_action>-index } )| ).
*    ENDIF.

  ENDMETHOD.

ENDCLASS.
