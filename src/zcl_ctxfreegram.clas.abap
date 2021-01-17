CLASS zcl_ctxfreegram DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES ty_u_symbol_type TYPE c LENGTH 1.
    TYPES : BEGIN OF ty_us_symbol,
              type   TYPE ty_u_symbol_type,
              index  TYPE sytabix,
              value  TYPE string,
              object TYPE REF TO zif_ctxfreegram_rule_elem,
            END OF ty_us_symbol.
    TYPES ty_ut_symbol TYPE STANDARD TABLE OF ty_us_symbol WITH NON-UNIQUE KEY type index value object.
    TYPES : BEGIN OF ty_us_rule,
              lhs                         TYPE REF TO zif_ctxfreegram_rule_nonterm,
              rhs                         TYPE REF TO zif_ctxfreegram_rule_elem,
              index                       TYPE sytabix,
              id                          TYPE string,
              lhs_nonterm_index           TYPE sytabix,
              name_alv                    TYPE string,
              plain_text                  TYPE string,
              t_symbol                    TYPE ty_ut_symbol, "no comment and no special instruction
              t_symbol_including_comments TYPE ty_ut_symbol,
            END OF ty_us_rule.
    TYPES ty_u_item_type TYPE c LENGTH 1.
    CONSTANTS : BEGIN OF lcs_item_type,
                  kernel  TYPE ty_u_item_type VALUE 'K',
                  closure TYPE ty_u_item_type VALUE 'C',
                END OF lcs_item_type.
    TYPES : BEGIN OF ty_us_item,
              rule_index        TYPE sytabix,
              lhs_nonterm_index TYPE sytabix,
              type              TYPE ty_u_item_type,
              dot_index         TYPE sytabix,
              dot_at_the_end    TYPE abap_bool,
              nr_of_rhs_symbols TYPE i,
              s_dot_symbol      TYPE ty_us_symbol,
            END OF ty_us_item.
    TYPES ty_ut_item TYPE STANDARD TABLE OF ty_us_item WITH DEFAULT KEY.
    TYPES : BEGIN OF ty_us_itemset,
              index                 TYPE sytabix,
              start_symbol          TYPE ty_us_symbol,
              t_item                TYPE ty_ut_item,
              rule_index            TYPE sytabix,
              rule_lhs              TYPE sytabix,
              index_of_reduced_rule TYPE sytabix,
            END OF ty_us_itemset.
    TYPES : BEGIN OF ty_us_transition,
              itemset_index      TYPE sytabix,
              s_symbol           TYPE ty_us_symbol,
              goto_itemset_index TYPE sytabix,
            END OF ty_us_transition.
    TYPES ty_u_action TYPE c LENGTH 1.
    TYPES : BEGIN OF ty_us_action,
              "! State / Item set number
              state          TYPE sytabix,
              "! Terminal number
              terminal_index TYPE sytabix,
              "! Shift, Reduce or Accept
              action         TYPE ty_u_action,
              "! If shift, it's the new state/item set, if reduce it's the rule number
              index          TYPE sytabix,
            END OF ty_us_action.
    TYPES : BEGIN OF ty_us_goto,
              "! State / Item set number
              state             TYPE sytabix,
              "! Nonterminal
              lhs_nonterm_index TYPE sytabix,
              "! Next State / Item set number after reducing nonterminal at given state
              next_state        TYPE sytabix,
            END OF ty_us_goto,
            ty_goto_table TYPE HASHED TABLE OF ty_us_goto WITH UNIQUE KEY state lhs_nonterm_index.
    TYPES : BEGIN OF ty_us_first_set,
              lhs_nonterm_index TYPE sytabix,
              s_rhs             TYPE zcl_ctxfreegram=>ty_us_symbol,
            END OF ty_us_first_set.
    TYPES : BEGIN OF ty_us_follow_set,
              symbol          TYPE zcl_ctxfreegram=>ty_us_symbol,
              t_follow_symbol TYPE zcl_ctxfreegram=>ty_ut_symbol,
            END OF ty_us_follow_set.
    TYPES: ty_ut_first_set TYPE STANDARD TABLE OF ty_us_first_set WITH NON-UNIQUE KEY lhs_nonterm_index s_rhs,
           BEGIN OF ty_rule,
             lhs TYPE REF TO zif_ctxfreegram_rule_nonterm,
             rhs TYPE REF TO zif_ctxfreegram_rule_elem,
           END OF ty_rule,
           ty_rules TYPE STANDARD TABLE OF ty_rule WITH EMPTY KEY.
    TYPES : BEGIN OF ty_us_terminal,
              index    TYPE sytabix,
              terminal TYPE string,
              object   TYPE REF TO zif_ctxfreegram_rule_elem,
            END OF ty_us_terminal.
    TYPES : BEGIN OF ty_us_nonterminal,
              index       TYPE sytabix,
              nonterminal TYPE string,
              object      TYPE REF TO zif_ctxfreegram_rule_elem,
            END OF ty_us_nonterminal.

    CONSTANTS : BEGIN OF lcs_action,
                  shift  TYPE ty_u_action VALUE 's',
                  reduce TYPE ty_u_action VALUE 'r',
                  accept TYPE ty_u_action VALUE 'a',
                END OF lcs_action.

    DATA: au_start_rule            TYPE REF TO zif_ctxfreegram_rule_nonterm READ-ONLY,
          aut_rule                 TYPE TABLE OF ty_us_rule READ-ONLY,
          aut_itemset              TYPE TABLE OF ty_us_itemset READ-ONLY,
          aut_transition           TYPE TABLE OF ty_us_transition READ-ONLY,
          aut_action               TYPE TABLE OF ty_us_action READ-ONLY,
          aut_goto                 TYPE ty_goto_table READ-ONLY,
          "! List of all possible terminals for each nonterminal, which may occur at the start of this nonterminal
          aut_first_set            TYPE ty_ut_first_set READ-ONLY,
          "! List of all possible terminals for each nonterminal, which may occur after this nonterminal
          aut_follow_set           TYPE TABLE OF ty_us_follow_set READ-ONLY WITH NON-UNIQUE KEY symbol, "lhs rhs_terminal_index rhs_follow_set,
          au_terminal_end_of_input TYPE sytabix READ-ONLY,
          au_starting_nonterminal  TYPE sytabix READ-ONLY.
    DATA aut_nonterminal  TYPE TABLE OF ty_us_nonterminal READ-ONLY.
    DATA aut_terminal     TYPE TABLE OF ty_us_terminal READ-ONLY.

    METHODS constructor
      IMPORTING
        start_rule TYPE REF TO zif_ctxfreegram_rule_nonterm
        rules      TYPE ty_rules
      RAISING
        zcx_ctxfreegram.

    METHODS render_itemsets RETURNING VALUE(result) TYPE string.
    METHODS render_transitions RETURNING VALUE(result) TYPE string.
    METHODS render_rules RETURNING VALUE(result) TYPE string.
    METHODS render_action_goto_tables RETURNING VALUE(result) TYPE string.

  PRIVATE SECTION.

    CONSTANTS : BEGIN OF lcs_symbol_type,
                  nonterminal      TYPE ty_u_symbol_type VALUE 'N',
                  terminal         TYPE ty_u_symbol_type VALUE 'T',
                  comment          TYPE ty_u_symbol_type VALUE '*',
                  special_sequence TYPE ty_u_symbol_type VALUE '?',
                  empty            TYPE ty_u_symbol_type VALUE 'E',
                END OF lcs_symbol_type.

*    TYPES : BEGIN OF ty_us_symbol2,
*              type  TYPE ty_u_symbol_type,
*              index TYPE sytabix,
*              value TYPE string,
*              object TYPE REF TO zif_ctxfreegram_rule_elem,
*            END OF ty_us_symbol2.
    TYPES : BEGIN OF ty_us_freetext,
              index TYPE sytabix,
              value TYPE string,
            END OF ty_us_freetext.
    TYPES : BEGIN OF ty_ls_lhs,
              nonterminal_index TYPE i,
              count             TYPE i, "p LENGTH 5 DECIMALS 0,
            END OF ty_ls_lhs.

    DATA aut_symbol       TYPE TABLE OF ty_us_symbol.
    DATA aut_freetext     TYPE TABLE OF ty_us_freetext.
*    DATA lt_lhs TYPE TABLE OF ty_ls_lhs WITH KEY nonterminal_index.
    DATA ai_ref_rule TYPE REF TO ty_us_rule.


*    CLASS-METHODS simplify_rules
*      IMPORTING
*        io_cfg        TYPE REF TO zcl_ctxfreegram
*      RETURNING
*        VALUE(ro_cfg) TYPE REF TO zcl_ctxfreegram.


*    METHODS calculate_redundant_data.
*    METHODS load_rules
*      IMPORTING
*        it_text TYPE STANDARD TABLE OPTIONAL
*        i_text  TYPE string OPTIONAL
*      RAISING
*        zcx_ctxfreegram.
    METHODS get_terminal
      IMPORTING
        i_index         TYPE sytabix
        i_abap          TYPE csequence DEFAULT abap_false
      RETURNING
        VALUE(r_result) TYPE string.
    METHODS get_nonterminal
      IMPORTING
        i_index         TYPE sytabix
      RETURNING
        VALUE(r_result) TYPE string.
    METHODS get_freetext
      IMPORTING
        i_index         TYPE sytabix
      RETURNING
        VALUE(r_result) TYPE string.
    METHODS get_symbol
      IMPORTING
        is_symbol       TYPE ty_us_symbol
      RETURNING
        VALUE(r_result) TYPE string.
    METHODS get_rules_as_text
      EXPORTING
        et_text   TYPE STANDARD TABLE
        e_text    TYPE string
        et_stream TYPE STANDARD TABLE.
    METHODS error.
    METHODS add_terminal
      IMPORTING
        terminal     TYPE REF TO zif_ctxfreegram_rule_term
      RETURNING
        VALUE(index) TYPE i.
    METHODS add_nonterminal
      IMPORTING
        nonterminal  TYPE REF TO zif_ctxfreegram_rule_nonterm
      RETURNING
        VALUE(index) TYPE i.
*    METHODS add_rule
*      IMPORTING
*        i_next_symbol TYPE csequence.

    TYPES ty_i_set_type TYPE i.
    CONSTANTS : BEGIN OF c_is_set_type,
                  first_set  TYPE ty_i_set_type VALUE 1,
                  follow_set TYPE ty_i_set_type VALUE 2,
                END OF c_is_set_type.
    TYPES : BEGIN OF ty_is_request,
              type   TYPE ty_i_set_type,
              symbol TYPE zcl_ctxfreegram=>ty_us_symbol,
            END OF ty_is_request.
    TYPES ty_it_request TYPE TABLE OF ty_is_request.
    TYPES ty_i_nonterminal  TYPE sytabix.
    TYPES ty_it_nonterminal TYPE TABLE OF sytabix.

    METHODS create_item_set_transit_tables.
    METHODS create_action_table.
    METHODS create_goto_table.
    METHODS create_first_and_follow_sets.
    METHODS create_first_or_follow_set
      IMPORTING
        is_request           TYPE ty_is_request
        it_processed_request TYPE ty_it_request OPTIONAL
      RETURNING
        VALUE(rt_symbol)     TYPE zcl_ctxfreegram=>ty_ut_symbol.

    METHODS create_first_set.
    METHODS create_first_set2
      IMPORTING
        i_nonterminal             TYPE ty_i_nonterminal
        it_preceding_nonterminals TYPE ty_it_nonterminal OPTIONAL
      RETURNING
        VALUE(rt_first_set)       TYPE ty_ut_first_set.
    METHODS create_follow_set.
    METHODS create_follow_set2
      IMPORTING is_symbol            TYPE zcl_ctxfreegram=>ty_us_symbol
                it_preceding_symbol  TYPE zcl_ctxfreegram=>ty_ut_symbol OPTIONAL
      RETURNING
                VALUE(rs_follow_set) TYPE ty_us_follow_set.
    METHODS remove_useless_reduce_actions.

    METHODS render_table
      IMPORTING
        table         TYPE ANY TABLE
      RETURNING
        VALUE(result) TYPE string.

    METHODS create_render_table
      IMPORTING
        terminals     TYPE abap_bool DEFAULT abap_true
        nonterminals  TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result) TYPE REF TO data.

ENDCLASS.



CLASS zcl_ctxfreegram IMPLEMENTATION.


  METHOD add_nonterminal.

    READ TABLE aut_nonterminal WITH KEY nonterminal = nonterminal->name ASSIGNING FIELD-SYMBOL(<ls_nonterminal>).
    IF sy-subrc <> 0.

      APPEND INITIAL LINE TO aut_nonterminal ASSIGNING <ls_nonterminal>.
      <ls_nonterminal>-index = sy-tabix.
      <ls_nonterminal>-nonterminal = nonterminal->name.
      <ls_nonterminal>-object = nonterminal.

      APPEND INITIAL LINE TO aut_symbol ASSIGNING FIELD-SYMBOL(<ls_symbol2>).
      <ls_symbol2>-type  = lcs_symbol_type-nonterminal.
      <ls_symbol2>-index = <ls_nonterminal>-index.
      <ls_symbol2>-value = <ls_nonterminal>-nonterminal.
      <ls_symbol2>-object = nonterminal.

    ENDIF.

    index = <ls_nonterminal>-index.

*  ENDMETHOD.
*
*  METHOD load_rules.
*    DATA: l_token           TYPE string,
*          l_grammar         TYPE string,
*          lt_match          TYPE match_result_tab,
*          l_state           TYPE i,
*          l_first2c         TYPE c LENGTH 2,
*          l_lhs             TYPE string,
*          ls_lhs            TYPE ty_ls_lhs,
*          lt_token          TYPE TABLE OF string,
*          l_new_rule_symbol TYPE string.
*    FIELD-SYMBOLS:
*      <ls_match>       TYPE match_result,
*      <ls_rule>        TYPE ty_us_rule,
*      <ls_nonterminal> TYPE ty_us_nonterminal,
*      <ls_symbol>      TYPE ty_us_symbol,
*      <ls_freetext>    TYPE ty_us_freetext,
*      <ls_symbol2>     TYPE ty_us_symbol2.
*
*    l_end_of_rule_symbol = '__'.
*    CLEAR ai_ref_rule.
*    CONCATENATE LINES OF it_text INTO l_grammar SEPARATED BY space.
*    IF i_text IS NOT INITIAL.
*      CONCATENATE l_grammar ` ` i_text INTO l_grammar.
*    ENDIF.
*    FIND ALL OCCURRENCES OF REGEX ''
*          & '"((?!"")[^"])*"|'        " terminal (example: "SELECT")
*          & ' |'                      " space
*          & '\(\*((?!\*\)).)*\*\)|'   " comment (* ... *)
*          & '\?[^?]*\?|'              " special instruction ? ... ?
*          & ';|'                      " end of production rule (;)
*          & '=|'                      " =
*          & ',|'                      " , element separator of LHS
*          & '[^ ;,=|{}]+'             " nonterminal : any character sequence which doesn't contain any space, semicolon, etc.
*          IN l_grammar
*          RESULTS lt_match.
*    REFRESH lt_token.
*    LOOP AT lt_match ASSIGNING <ls_match>.
*      l_token = l_grammar+<ls_match>-offset(<ls_match>-length).
*      IF NOT ( l_token IS INITIAL OR l_token CO space ).
*        APPEND l_token TO lt_token.
*      ENDIF.
*    ENDLOOP.
*    FREE lt_match.
*    APPEND INITIAL LINE TO aut_rule REFERENCE INTO ai_ref_rule.
*    ASSIGN ai_ref_rule->* TO <ls_rule>.
*    l_state = 0.
*    LOOP AT lt_token INTO l_token.
*      IF <ls_rule>-plain_text IS INITIAL.
*        " start of production rule
*        <ls_rule>-plain_text = l_token.
*      ELSE.
*        CONCATENATE <ls_rule>-plain_text ` ` l_token INTO <ls_rule>-plain_text.
*      ENDIF.
*      l_first2c = l_token.
*      IF l_first2c = '(*'    "comment
*            OR l_first2c(1) = '?'. "special sequence
*        IF ai_ref_rule IS BOUND.
*          ASSIGN ai_ref_rule->* TO <ls_rule>.
*          APPEND INITIAL LINE TO aut_freetext ASSIGNING <ls_freetext>.
*          <ls_freetext>-index = sy-tabix.
*          <ls_freetext>-value = l_token.
*          APPEND INITIAL LINE TO <ls_rule>-t_symbol ASSIGNING <ls_symbol>.
*          <ls_symbol>-index = <ls_freetext>-index.
*          IF l_first2c = '(*'.
*            <ls_symbol>-type = lcs_symbol_type-comment.
*          ELSEIF l_first2c(1) = '?'.
*            <ls_symbol>-type = lcs_symbol_type-special_sequence.
*          ENDIF.
*          APPEND INITIAL LINE TO aut_symbol ASSIGNING <ls_symbol2>.
*          <ls_symbol2>-type  = <ls_symbol>-type.
*          <ls_symbol2>-index = <ls_freetext>-index.
*          <ls_symbol2>-value = <ls_freetext>-value.
*        ENDIF.
*      ELSE.
*        CLEAR l_new_rule_symbol.
*        CASE l_state.
*          WHEN 0. "start of production rule
*            IF l_first2c(1) = '"' "terminal
*                  OR l_first2c = '='  "assignment
*                  OR l_first2c = ','  "element separator in a sequence
*                  OR l_first2c = ';'. "end of a production rule
*              RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING textid = zcx_ctxfreegram=>rule_first_character.
*            ELSE.
*              l_lhs = l_token. "LHS de la production rule
*              READ TABLE aut_nonterminal WITH KEY nonterminal = l_lhs ASSIGNING <ls_nonterminal>.
*              IF sy-subrc <> 0.
*                APPEND INITIAL LINE TO aut_nonterminal ASSIGNING <ls_nonterminal>.
*                <ls_nonterminal>-index = sy-tabix.
*                <ls_nonterminal>-nonterminal = l_lhs.
*                APPEND INITIAL LINE TO aut_symbol ASSIGNING <ls_symbol2>.
*                <ls_symbol2>-type  = lcs_symbol_type-nonterminal.
*                <ls_symbol2>-index = <ls_nonterminal>-index.
*                <ls_symbol2>-value = <ls_nonterminal>-nonterminal.
*              ENDIF.
*              <ls_rule>-lhs_nonterm_index = <ls_nonterminal>-index.
*              l_state = 1.
*            ENDIF.
*          WHEN 1. "just after LHS
*            IF l_first2c = '='. "affectation
*              l_state = 2.
*            ELSE.
*              RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING textid = zcx_ctxfreegram=>equal_expected_after_lhs.
*            ENDIF.
*          WHEN 2. "start of RHS (just after "LHS =")
*            IF l_first2c = '='  "affectation
*                  OR l_first2c = ','. "element separator in a sequence
*              RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING textid = zcx_ctxfreegram=>character_after_lhs_equal.
*            ELSEIF l_first2c = ';'.
*              " empty production rule
*              l_state = 0.
*            ELSE.
*              add_rule( i_next_symbol = l_token ).
*              l_state = 3.
*            ENDIF.
*          WHEN 3. "just after an RHS element
*            IF l_first2c = ','. "element separator in a sequence
*              l_state = 2.
*            ELSEIF l_first2c = ';'. "end of a production rule
*              l_state = 0.
*            ELSE.
*              RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING textid = zcx_ctxfreegram=>after_rhs_element.
*            ENDIF.
*        ENDCASE.
*        IF l_state = 0.
*          APPEND INITIAL LINE TO aut_rule REFERENCE INTO ai_ref_rule.
*          ASSIGN ai_ref_rule->* TO <ls_rule>.
*          l_new_rule_symbol = '__'.
*          l_new_rule_symbol = l_token.
*          l_new_rule_symbol = '__'.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*    " delete last added production rule if it's empty
*    IF ai_ref_rule IS BOUND.
*      ASSIGN ai_ref_rule->* TO <ls_rule>.
*      IF <ls_rule> IS INITIAL.
*        DESCRIBE TABLE aut_rule.
*        DELETE aut_rule INDEX sy-tfill.
*      ENDIF.
*    ENDIF.
*    calculate_redundant_data( ).
*    LOOP AT aut_rule ASSIGNING <ls_rule>.
*      <ls_rule>-t_symbol_including_comments = <ls_rule>-t_symbol.
*      DELETE <ls_rule>-t_symbol WHERE type = lcs_symbol_type-comment
*                                    OR type = lcs_symbol_type-special_sequence.
*    ENDLOOP.
*  ENDMETHOD.
*
*  METHOD simplify_rules.
*    TYPES : BEGIN OF ty_ls_subst,
*              n1          TYPE i, "nonterminal
*              symbol      TYPE zcl_ctxfreegram=>ty_us_symbol, "terminal ou nonterminal
*              subst_count TYPE i,
*            END OF ty_ls_subst.
*    DATA: ls_subst TYPE ty_ls_subst,
*          lt_subst TYPE TABLE OF ty_ls_subst,
*          l_count  TYPE i,
*          ls_lhs   TYPE ty_ls_lhs.
*    FIELD-SYMBOLS:
*      <ls_subst>  TYPE ty_ls_subst,
*      <ls_rule>   TYPE zcl_ctxfreegram=>ty_us_rule,
*      <ls_symbol> TYPE zcl_ctxfreegram=>ty_us_symbol.
*
*    " create new object
*    CREATE OBJECT ro_cfg.
*    ro_cfg->aut_rule        = io_cfg->aut_rule.
*    ro_cfg->aut_symbol      = io_cfg->aut_symbol.
*    ro_cfg->aut_nonterminal = io_cfg->aut_nonterminal.
*    ro_cfg->aut_terminal    = io_cfg->aut_terminal.
*    ro_cfg->aut_freetext    = io_cfg->aut_freetext.
*    ro_cfg->lt_lhs          = io_cfg->lt_lhs.
*    CLEAR ls_subst.
*    REFRESH lt_subst.
*    LOOP AT ro_cfg->lt_lhs INTO ls_lhs WHERE count = 1.
*      LOOP AT ro_cfg->aut_rule ASSIGNING <ls_rule>
*            FROM 2 "in any case do not consider start element
*            WHERE lhs_nonterm_index = ls_lhs-nonterminal_index.
*        CLEAR ls_subst.
*        ls_subst-n1 = ls_lhs-nonterminal_index.
*        " this production rule contains only one nonterminal (N1 = N2), so all N1 instances
*        " may be replaced with N2
*        IF lines( <ls_rule>-t_symbol ) = 1.
*          READ TABLE <ls_rule>-t_symbol INDEX 1 ASSIGNING <ls_symbol>.
*          IF sy-subrc = 0.
*            ls_subst-symbol = <ls_symbol>.
*            APPEND ls_subst TO lt_subst.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.
*    " There may be N1 = N2 and N2 = N3, in that case transform
*    " them into N1 = N3 and N2 = N3
*    " TODO manage endless loops (N1 = N2 and N2 = N1)
*    DATA l_changed TYPE abap_bool.
*    DO.
*      l_changed = abap_false.
*      LOOP AT lt_subst ASSIGNING <ls_subst>
*            WHERE symbol-type = zcl_ctxfreegram=>lcs_symbol_type-nonterminal.
*        READ TABLE lt_subst INTO ls_subst WITH KEY n1 = <ls_subst>-symbol-index.
*        IF sy-subrc = 0.
*          <ls_subst>-symbol = ls_subst-symbol.
*          l_changed = abap_true.
*        ENDIF.
*      ENDLOOP.
*      IF l_changed = abap_false.
*        EXIT.
*      ENDIF.
*    ENDDO.
*
*    " replace all useless RHS non-terminals
*    LOOP AT ro_cfg->aut_rule ASSIGNING <ls_rule>.
*      LOOP AT <ls_rule>-t_symbol ASSIGNING <ls_symbol>
*            WHERE type = lcs_symbol_type-nonterminal.
*        READ TABLE lt_subst ASSIGNING <ls_subst>
*              WITH KEY n1 = <ls_symbol>-index.
*        IF sy-subrc = 0.
*          <ls_symbol> = <ls_subst>-symbol.
*          ADD 1 TO <ls_subst>-subst_count.
*        ENDIF.
*      ENDLOOP.
*      LOOP AT <ls_rule>-t_symbol_including_comments ASSIGNING <ls_symbol>
*            WHERE type = lcs_symbol_type-nonterminal.
*        READ TABLE lt_subst ASSIGNING <ls_subst>
*              WITH KEY n1 = <ls_symbol>-index.
*        IF sy-subrc = 0.
*          <ls_symbol> = <ls_subst>-symbol.
*          ADD 1 TO <ls_subst>-subst_count.
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.
*
*    " delete RHS useless nonterminals of production rules;
*    "   in that case, the production rules must be renumbered
*    LOOP AT lt_subst INTO ls_subst WHERE subst_count >= 1.
*      DELETE ro_cfg->aut_rule         WHERE lhs_nonterm_index = ls_subst-n1.
*      DELETE ro_cfg->lt_lhs           WHERE nonterminal_index = ls_subst-n1.
*      DELETE ro_cfg->aut_nonterminal  WHERE index = ls_subst-n1.
*      DELETE ro_cfg->aut_symbol       WHERE type = lcs_symbol_type-nonterminal AND index = ls_subst-n1.
*    ENDLOOP.
*
*    ro_cfg->calculate_redundant_data( ).

  ENDMETHOD.


  METHOD add_terminal.

    READ TABLE aut_terminal WITH KEY terminal = terminal->value ASSIGNING FIELD-SYMBOL(<ls_terminal>).
    IF sy-subrc <> 0.

      APPEND INITIAL LINE TO aut_terminal ASSIGNING <ls_terminal>.
      <ls_terminal>-index    = sy-tabix.
      <ls_terminal>-terminal = terminal->value.
      <ls_terminal>-object   = terminal.

      APPEND INITIAL LINE TO aut_symbol ASSIGNING FIELD-SYMBOL(<ls_symbol2>).
      <ls_symbol2>-type   = lcs_symbol_type-terminal.
      <ls_symbol2>-index  = <ls_terminal>-index.
      <ls_symbol2>-value  = <ls_terminal>-terminal.
      <ls_symbol2>-object = terminal.

    ENDIF.

    index = <ls_terminal>-index.

*  ENDMETHOD.
*
*
*  METHOD calculate_redundant_data.
*    DATA: ls_lhs  TYPE ty_ls_lhs,
*          l_count TYPE i,
*          l_text  TYPE string.
*    DATA lt_lhs TYPE TABLE OF ty_ls_lhs WITH KEY nonterminal_index.
*    FIELD-SYMBOLS:
*      <ls_rule>        TYPE zcl_ctxfreegram=>ty_us_rule,
*      <ls_nonterminal> TYPE zcl_ctxfreegram=>ty_us_nonterminal.
*
**    LOOP AT aut_rule ASSIGNING <ls_rule>.
**      <ls_rule>-index = sy-tabix.
**    ENDLOOP.
*    CLEAR ls_lhs.
*    REFRESH lt_lhs.
*    LOOP AT aut_rule ASSIGNING <ls_rule>.
*      ls_lhs-nonterminal_index = <ls_rule>-lhs_nonterm_index.
*      ls_lhs-count = 1.
*      COLLECT ls_lhs INTO lt_lhs.
*    ENDLOOP.
*    LOOP AT lt_lhs INTO ls_lhs.
*      READ TABLE aut_nonterminal WITH KEY index = ls_lhs-nonterminal_index ASSIGNING <ls_nonterminal>.
*      ASSERT sy-subrc = 0.
*      IF ls_lhs-count = 1.
*        READ TABLE aut_rule
*              WITH KEY lhs_nonterm_index = ls_lhs-nonterminal_index
*              ASSIGNING <ls_rule>.
*        ASSERT sy-subrc = 0.
*        <ls_rule>-name_alv = ls_lhs-nonterminal_index.
*        CONDENSE <ls_rule>-name_alv NO-GAPS.
*        <ls_rule>-id = <ls_nonterminal>-nonterminal.
*      ELSEIF ls_lhs-count >= 2.
*        l_count = 0.
*        LOOP AT aut_rule ASSIGNING <ls_rule>
*              WHERE lhs_nonterm_index = ls_lhs-nonterminal_index.
*          <ls_rule>-name_alv = 'p1.p2'.
*          ADD 1 TO l_count.
*          l_text = l_count.
*          CONDENSE l_text NO-GAPS.
*          REPLACE 'p2' IN <ls_rule>-name_alv WITH l_text.
*          CONCATENATE <ls_nonterminal>-nonterminal '_' l_text INTO <ls_rule>-id.
*          l_text = ls_lhs-nonterminal_index.
*          CONDENSE l_text NO-GAPS.
*          REPLACE 'p1' IN <ls_rule>-name_alv WITH l_text.
*        ENDLOOP.
*      ELSE.
*        ASSERT 0 = 1.
*      ENDIF.
*    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    TYPES ty_elements TYPE STANDARD TABLE OF REF TO zif_ctxfreegram_rule_elem WITH EMPTY KEY.
    FIELD-SYMBOLS:
      <ls_terminal> TYPE zcl_ctxfreegram=>ty_us_terminal,
      <ls_symbol2>  TYPE ty_us_symbol.

    IF start_rule IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING error = zcx_ctxfreegram=>c_error-start_rule_not_bound.
    ENDIF.

    au_start_rule = start_rule.
    au_terminal_end_of_input = add_terminal( zcl_ctxfreegram_factory=>create( )->new_terminal( terminal = '$' ) ).

    IF NOT line_exists( rules[ lhs = start_rule ] ).
      RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING error = zcx_ctxfreegram=>c_error-missing_start_rule.
    ENDIF.

    LOOP AT rules INTO DATA(rule).

      IF rule-lhs IS NOT BOUND.
        RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING error = zcx_ctxfreegram=>c_error-lhs_element_not_bound.
      ENDIF.

      APPEND INITIAL LINE TO aut_rule ASSIGNING FIELD-SYMBOL(<ls_rule>).
      <ls_rule>-lhs = rule-lhs.
      <ls_rule>-rhs = rule-rhs.
      <ls_rule>-index = sy-tabix.
      <ls_rule>-lhs_nonterm_index = add_nonterminal( rule-lhs ).

      IF rule-lhs = start_rule.
        IF au_starting_nonterminal IS NOT INITIAL.
          RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING error = zcx_ctxfreegram=>c_error-start_rule_must_appear_once.
        ENDIF.
        au_starting_nonterminal = <ls_rule>-lhs_nonterm_index.
      ENDIF.

      IF rule-rhs IS BOUND.
        DATA(elements) = VALUE ty_elements( ( rule-rhs ) ).
        LOOP AT elements INTO DATA(element).

          IF element IS NOT BOUND.
            RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING error = zcx_ctxfreegram=>c_error-rhs_element_not_bound.
          ENDIF.

          DATA(new_elements) = VALUE ty_elements( ).

          CASE element->type.
            WHEN element->c_type-nonterminal.
              DATA(nonterminal) = CAST zif_ctxfreegram_rule_nonterm( element ).
              <ls_rule>-t_symbol = VALUE #(
                  BASE <ls_rule>-t_symbol
                  ( index  = add_nonterminal( nonterminal )
                    type   = lcs_symbol_type-nonterminal
                    object = nonterminal ) ).
            WHEN element->c_type-terminal.
              DATA(terminal) = CAST zif_ctxfreegram_rule_term( element ).
              <ls_rule>-t_symbol = VALUE #(
                  BASE <ls_rule>-t_symbol
                  ( index  = add_terminal( terminal )
                    type   = lcs_symbol_type-terminal
                    object = terminal ) ).
            WHEN element->c_type-sequence.
              DATA(sequence) = CAST zif_ctxfreegram_rule_seq( element ).
              new_elements = VALUE #( FOR aux_element IN sequence->elements ( aux_element ) ).
          ENDCASE.

          LOOP AT new_elements INTO element.
            IF line_exists( elements[ table_line = element ] ).
              DELETE new_elements.
            ENDIF.
          ENDLOOP.
          APPEND LINES OF new_elements TO elements.

        ENDLOOP.
      ENDIF.

    ENDLOOP.

    create_first_and_follow_sets( ).
    create_item_set_transit_tables( ).
    create_action_table( ).
    create_goto_table( ).

  ENDMETHOD.


  METHOD create_action_table.
    DATA: l_rule_index    TYPE sytabix.
    FIELD-SYMBOLS:
      <ls_transition> TYPE ty_us_transition,
      <ls_action>     TYPE ty_us_action,
      <ls_itemset>    TYPE ty_us_itemset,
      <ls_item>       TYPE ty_us_item,
      <ls_follow_set> TYPE ty_us_follow_set,
      <ls_symbol>     TYPE zcl_ctxfreegram=>ty_us_symbol,
      <ls_rule>       TYPE zcl_ctxfreegram=>ty_us_rule,
      <ls_terminal>   TYPE zcl_ctxfreegram=>ty_us_terminal.

    REFRESH aut_action.

    " Part 1 of action determination = list of SHIFT
    "   SIMPLEST WAY : VIA THE TRANSITION TABLE
    "  (maybe this other solution :
    "   - there's no SHIFT if dot is at the end of RHS
    "   - If dot is not at the end of RHS:
    "        - if symbol at dot is a terminal, it's this terminal
    "        - if it's a nonterminal, there's a shift for all symbols of the First Set of this nonterminal except "$"
    "  )
    LOOP AT aut_transition ASSIGNING <ls_transition>
          WHERE s_symbol-type = zcl_ctxfreegram=>lcs_symbol_type-terminal.
      APPEND INITIAL LINE TO aut_action ASSIGNING <ls_action>.
      <ls_action>-state           = <ls_transition>-itemset_index.
      <ls_action>-terminal_index  = <ls_transition>-s_symbol-index.
      <ls_action>-action          = lcs_action-shift.
      <ls_action>-index           = <ls_transition>-goto_itemset_index.
    ENDLOOP.

    " Part 2 of action determination  = list of REDUCE
    "   - there's none if the dot is at the end of RHS
    "   - If dot is not at the end of RHS:
    "        - if symbol at dot is a terminal, it's this terminal
    "        - if it's a nonterminal, take all symbols of the First Set of this nonterminal except "$"
    LOOP AT aut_itemset ASSIGNING <ls_itemset>.
      LOOP AT <ls_itemset>-t_item ASSIGNING <ls_item>
            WHERE type = lcs_item_type-kernel
              AND s_dot_symbol-type = zcl_ctxfreegram=>lcs_symbol_type-nonterminal.
        LOOP AT aut_rule ASSIGNING <ls_rule>
              WHERE lhs_nonterm_index = <ls_item>-s_dot_symbol-index
                AND t_symbol IS INITIAL.
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0 AND 0 = lines( <ls_rule>-t_symbol ).
          LOOP AT aut_follow_set ASSIGNING <ls_follow_set> WHERE symbol = <ls_item>-s_dot_symbol.
            LOOP AT <ls_follow_set>-t_follow_symbol ASSIGNING <ls_symbol>.
              APPEND INITIAL LINE TO aut_action ASSIGNING <ls_action>.
              <ls_action>-state           = <ls_itemset>-index.
              <ls_action>-terminal_index  = <ls_symbol>-index.
              <ls_action>-action          = lcs_action-reduce.
              <ls_action>-index           = <ls_rule>-index.
            ENDLOOP.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

      LOOP AT <ls_itemset>-t_item ASSIGNING <ls_item>
            WHERE type = lcs_item_type-kernel
              AND dot_at_the_end = abap_true.
        READ TABLE aut_follow_set ASSIGNING <ls_follow_set>
              WITH KEY  symbol-type  = zcl_ctxfreegram=>lcs_symbol_type-nonterminal
                        symbol-index = <ls_item>-lhs_nonterm_index.
        ASSERT sy-subrc = 0.
        LOOP AT <ls_follow_set>-t_follow_symbol ASSIGNING <ls_symbol>.
          APPEND INITIAL LINE TO aut_action ASSIGNING <ls_action>.
          <ls_action>-state           = <ls_itemset>-index.
          <ls_action>-terminal_index  = <ls_symbol>-index.
          IF <ls_item>-rule_index = 1.
            <ls_action>-action          = lcs_action-accept.
            <ls_action>-index           = 0.
          ELSE.
            <ls_action>-action          = lcs_action-reduce.
            <ls_action>-index           = <ls_item>-rule_index.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    " if both SHIFT and reduce are possible, then keep SHIFT
    LOOP AT aut_action ASSIGNING <ls_action> WHERE action = lcs_action-reduce.
      READ TABLE aut_action WITH KEY  state           = <ls_action>-state
                                      terminal_index  = <ls_action>-terminal_index
                                      action          = lcs_action-shift
                            TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE aut_action.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_first_and_follow_sets.
    DATA: ls_request    TYPE ty_is_request,
          ls_first_set  TYPE ty_us_first_set,
          ls_symbol     TYPE zcl_ctxfreegram=>ty_us_symbol,
          lt_symbol     TYPE zcl_ctxfreegram=>ty_ut_symbol,
          ls_follow_set TYPE ty_us_follow_set.
    FIELD-SYMBOLS:
          <ls_nonterminal>      TYPE zcl_ctxfreegram=>ty_us_nonterminal.
    FIELD-SYMBOLS:
      <ls_symbol>  TYPE zcl_ctxfreegram=>ty_us_symbol,
      <ls_symbol2> TYPE zcl_ctxfreegram=>ty_us_symbol.

    REFRESH aut_first_set.
    REFRESH aut_follow_set.

    LOOP AT aut_symbol ASSIGNING <ls_symbol2>
          WHERE type = zcl_ctxfreegram=>lcs_symbol_type-terminal
            OR  type = zcl_ctxfreegram=>lcs_symbol_type-nonterminal.

      CLEAR ls_request.
      ls_request-symbol-type  = <ls_symbol2>-type.
      ls_request-symbol-index = <ls_symbol2>-index.

      IF <ls_symbol2>-type = zcl_ctxfreegram=>lcs_symbol_type-nonterminal.
        ls_request-type = c_is_set_type-first_set.
        lt_symbol = create_first_or_follow_set( ls_request ).
        LOOP AT lt_symbol ASSIGNING <ls_symbol>.
          CLEAR ls_first_set.
          ls_first_set-lhs_nonterm_index = <ls_symbol2>-index.
          ls_first_set-s_rhs-type        = <ls_symbol>-type.
          ls_first_set-s_rhs-index       = <ls_symbol>-index.
          APPEND ls_first_set TO aut_first_set.
        ENDLOOP.
      ENDIF.

      ls_request-type = c_is_set_type-follow_set.
      lt_symbol = create_first_or_follow_set( ls_request ).
      CLEAR ls_follow_set.
      ls_follow_set-symbol-type   = <ls_symbol2>-type.
      ls_follow_set-symbol-index  = <ls_symbol2>-index.
      LOOP AT lt_symbol ASSIGNING <ls_symbol>.
        COLLECT <ls_symbol> INTO ls_follow_set-t_follow_symbol.
      ENDLOOP.
      APPEND ls_follow_set TO aut_follow_set.

    ENDLOOP.

  ENDMETHOD.


  METHOD create_first_or_follow_set.

    DATA: ls_request           TYPE ty_is_request,
          lt_processed_request TYPE ty_it_request.

    " ignore a request already processed (avoid recursive)
    READ TABLE it_processed_request WITH KEY table_line = is_request TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      EXIT.
    ENDIF.
    " used to avoid recursive
    lt_processed_request = it_processed_request.
    APPEND is_request TO lt_processed_request.



    CASE is_request-type.

      WHEN c_is_set_type-first_set.
        "----------------------------------
        " FIRST SET
        "----------------------------------

        DATA: at_least_one_rule_epsilon TYPE abap_bool,
              only_epsilons             TYPE abap_bool,
              ls_first_set              TYPE ty_us_first_set,
              lt_first_set              TYPE ty_ut_first_set,
              lt_symbol                 TYPE zcl_ctxfreegram=>ty_ut_symbol.
        FIELD-SYMBOLS:
          <ls_terminal>    TYPE zcl_ctxfreegram=>ty_us_terminal,
          <ls_nonterminal> TYPE zcl_ctxfreegram=>ty_us_nonterminal,
          <ls_first_set>   TYPE ty_us_first_set,
          <ls_rule>        TYPE zcl_ctxfreegram=>ty_us_rule,
          <ls_symbol>      TYPE zcl_ctxfreegram=>ty_us_symbol,
          <ls_symbol_bis>  TYPE zcl_ctxfreegram=>ty_us_symbol.

        at_least_one_rule_epsilon = abap_false.
        LOOP AT aut_rule ASSIGNING <ls_rule>
              WHERE lhs_nonterm_index = is_request-symbol-index.
          only_epsilons = abap_true.
          LOOP AT <ls_rule>-t_symbol ASSIGNING <ls_symbol>.
            IF <ls_symbol>-type = zcl_ctxfreegram=>lcs_symbol_type-nonterminal.
              CLEAR ls_request.
              ls_request-type   = c_is_set_type-first_set.
              ls_request-symbol = <ls_symbol>.
              lt_symbol = create_first_or_follow_set( is_request           = ls_request
                                                      it_processed_request = lt_processed_request ).
              LOOP AT lt_symbol ASSIGNING <ls_symbol_bis>.
                COLLECT <ls_symbol_bis> INTO rt_symbol.
              ENDLOOP.
            ELSEIF <ls_symbol>-type = zcl_ctxfreegram=>lcs_symbol_type-terminal
                  AND <ls_symbol>-index = au_terminal_end_of_input.
              " Process next symbol in the RHS, its First Set will make the First Set of I_NONTERMINAL
              CONTINUE.
            ELSE. "terminal
              COLLECT <ls_symbol> INTO rt_symbol.
            ENDIF.
            only_epsilons = abap_false.
            EXIT.
          ENDLOOP.
          IF abap_true = only_epsilons.
            at_least_one_rule_epsilon = abap_true.
          ENDIF.
        ENDLOOP.
        IF abap_true = at_least_one_rule_epsilon.
          " Add to the First set of requested NONTERMINAL the follow set of NONTERMINAL
          CLEAR ls_request.
          ls_request-type   = c_is_set_type-follow_set.
          ls_request-symbol = is_request-symbol.
          lt_symbol = create_first_or_follow_set( is_request           = ls_request
                                                  it_processed_request = lt_processed_request ).
          LOOP AT lt_symbol ASSIGNING <ls_symbol>.
            COLLECT <ls_symbol> INTO rt_symbol.
          ENDLOOP.
        ENDIF.

      WHEN c_is_set_type-follow_set.
        "----------------------------------
        " FOLLOW SET
        "----------------------------------

        DATA: ls_follow_set TYPE ty_us_follow_set,
              ls_symbol     TYPE zcl_ctxfreegram=>ty_us_symbol,
              l_tabix       TYPE sytabix.
        FIELD-SYMBOLS:
          <ls_follow_set> TYPE ty_us_follow_set,
          <ls_symbol2>    TYPE zcl_ctxfreegram=>ty_us_symbol.

        IF is_request-symbol-type = zcl_ctxfreegram=>lcs_symbol_type-nonterminal
              AND is_request-symbol-index = au_starting_nonterminal.
          CLEAR ls_symbol.
          ls_symbol-type   = zcl_ctxfreegram=>lcs_symbol_type-terminal.
          ls_symbol-index  = au_terminal_end_of_input.
          COLLECT ls_symbol INTO rt_symbol.
        ELSE.

          LOOP AT aut_rule ASSIGNING <ls_rule>.
            " for all terminals and nonterminals of RHS, the FOLLOW SET must be determined
            " except for symbol "end of input".
            LOOP AT <ls_rule>-t_symbol ASSIGNING <ls_symbol>
                  WHERE type  = is_request-symbol-type
                    AND index = is_request-symbol-index.
              IF sy-tabix = lines( <ls_rule>-t_symbol ).
                " if symbol is at the end of RHS, then take the follow-set of LHS of the production rule
                CLEAR ls_request.
                ls_request-type         = c_is_set_type-follow_set.
                ls_request-symbol-type  = zcl_ctxfreegram=>lcs_symbol_type-nonterminal.
                ls_request-symbol-index = <ls_rule>-lhs_nonterm_index.
                lt_symbol = create_first_or_follow_set( is_request           = ls_request
                                                        it_processed_request = lt_processed_request ).
                LOOP AT lt_symbol ASSIGNING <ls_symbol_bis>.
                  COLLECT <ls_symbol_bis> INTO rt_symbol.
                ENDLOOP.
              ELSE.
                " if symbol is NOT at the end of RHS, then based on the type of the next symbol:
                "   - if it's a terminal, add it to the follow-set
                "   - if it's a nonterminal, add its whole first-set to the follow-set
                l_tabix = sy-tabix + 1.
                READ TABLE <ls_rule>-t_symbol INDEX l_tabix ASSIGNING <ls_symbol2>.
                ASSERT sy-subrc = 0.
                IF <ls_symbol2>-type = zcl_ctxfreegram=>lcs_symbol_type-terminal.
                  COLLECT <ls_symbol2> INTO rt_symbol.
                ELSE.
                  CLEAR ls_request.
                  ls_request-type         = c_is_set_type-first_set.
                  ls_request-symbol-type  = <ls_symbol2>-type.
                  ls_request-symbol-index = <ls_symbol2>-index.
                  lt_symbol = create_first_or_follow_set( is_request           = ls_request
                                                          it_processed_request = lt_processed_request ).
                  LOOP AT lt_symbol ASSIGNING <ls_symbol_bis>.
                    COLLECT <ls_symbol_bis> INTO rt_symbol.
                  ENDLOOP.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDLOOP.
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD create_first_set.
    DATA: lt_first_set TYPE ty_ut_first_set.
    FIELD-SYMBOLS:
          <ls_nonterminal>  TYPE zcl_ctxfreegram=>ty_us_nonterminal.

    REFRESH aut_first_set.
    LOOP AT aut_nonterminal ASSIGNING <ls_nonterminal>.
      lt_first_set = create_first_set2( <ls_nonterminal>-index ).
      APPEND LINES OF lt_first_set TO aut_first_set.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_first_set2.
    DATA: lt_preceding_nonterminals TYPE ty_it_nonterminal,
          at_least_one_rule_epsilon TYPE abap_bool,
          only_epsilons             TYPE abap_bool,
          ls_first_set              TYPE ty_us_first_set,
          lt_first_set              TYPE ty_ut_first_set.
    FIELD-SYMBOLS:
      <ls_terminal>    TYPE zcl_ctxfreegram=>ty_us_terminal,
      <ls_nonterminal> TYPE zcl_ctxfreegram=>ty_us_nonterminal,
      <ls_first_set>   TYPE ty_us_first_set,
      <ls_rule>        TYPE zcl_ctxfreegram=>ty_us_rule,
      <ls_symbol>      TYPE zcl_ctxfreegram=>ty_us_symbol.

    " used to avoid recursive
    lt_preceding_nonterminals = it_preceding_nonterminals.
    APPEND i_nonterminal TO lt_preceding_nonterminals.

    at_least_one_rule_epsilon = abap_true.
    LOOP AT aut_rule ASSIGNING <ls_rule>
          WHERE lhs_nonterm_index = i_nonterminal.
      only_epsilons = abap_true.
      LOOP AT <ls_rule>-t_symbol ASSIGNING <ls_symbol>.
        IF <ls_symbol>-type = zcl_ctxfreegram=>lcs_symbol_type-nonterminal.
          " ignore a nonterminal already processed
          READ TABLE lt_preceding_nonterminals WITH KEY table_line = <ls_symbol>-index TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            lt_first_set = create_first_set2( i_nonterminal             = <ls_symbol>-index
                                              it_preceding_nonterminals = lt_preceding_nonterminals ).
            LOOP AT lt_first_set INTO ls_first_set.
              ls_first_set-lhs_nonterm_index = <ls_rule>-lhs_nonterm_index.
              COLLECT ls_first_set INTO rt_first_set.
            ENDLOOP.
          ENDIF.
        ELSEIF <ls_symbol>-type = zcl_ctxfreegram=>lcs_symbol_type-terminal
              AND <ls_symbol>-index = au_terminal_end_of_input.
          " Process next symbol in the RHS, its First Set will make the First Set of I_NONTERMINAL
          CONTINUE.
        ELSE. "terminal
          CLEAR ls_first_set.
          ls_first_set-lhs_nonterm_index = <ls_rule>-lhs_nonterm_index.
          ls_first_set-s_rhs-type        = <ls_symbol>-type.
          ls_first_set-s_rhs-index       = <ls_symbol>-index.
          COLLECT ls_first_set INTO rt_first_set.
        ENDIF.
        only_epsilons = abap_false.
        EXIT.
      ENDLOOP.
      IF abap_true = only_epsilons.
        at_least_one_rule_epsilon = abap_true.
      ENDIF.
    ENDLOOP.
    IF abap_true = at_least_one_rule_epsilon.
      " Add to the First set of I_NONTERMINAL the follow set of NONTERMINAL
      ASSERT 1 = 1.
    ENDIF.

  ENDMETHOD.


  METHOD create_follow_set.
    DATA: ls_symbol     TYPE zcl_ctxfreegram=>ty_us_symbol,
          ls_follow_set TYPE ty_us_follow_set.
    FIELD-SYMBOLS:
          <ls_symbol2>              TYPE zcl_ctxfreegram=>ty_us_symbol.

    REFRESH aut_follow_set.
    LOOP AT aut_symbol ASSIGNING <ls_symbol2>
          WHERE type = zcl_ctxfreegram=>lcs_symbol_type-terminal
            OR  type = zcl_ctxfreegram=>lcs_symbol_type-nonterminal.
      ls_symbol-type   = <ls_symbol2>-type.
      ls_symbol-index  = <ls_symbol2>-index.
      ls_follow_set = create_follow_set2( is_symbol = ls_symbol ).
      APPEND ls_follow_set TO aut_follow_set.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_follow_set2.
    DATA: ls_follow_set       TYPE ty_us_follow_set,
          ls_symbol           TYPE zcl_ctxfreegram=>ty_us_symbol,
          lt_first_set        TYPE ty_ut_first_set,
          lt_preceding_symbol TYPE zcl_ctxfreegram=>ty_ut_symbol,
          l_tabix             TYPE sytabix.
    FIELD-SYMBOLS:
      <ls_follow_set> TYPE ty_us_follow_set,
      <ls_rule>       TYPE zcl_ctxfreegram=>ty_us_rule,
      <ls_symbol>     TYPE zcl_ctxfreegram=>ty_us_symbol,
      <ls_symbol2>    TYPE zcl_ctxfreegram=>ty_us_symbol,
      <ls_first_set>  TYPE ty_us_first_set.

    " find the symbol in RHS of all the rules
    rs_follow_set-symbol = is_symbol.
    rs_follow_set-symbol-index  = is_symbol-index.

    IF is_symbol-type = zcl_ctxfreegram=>lcs_symbol_type-nonterminal
          AND is_symbol-index = au_starting_nonterminal.
      ls_symbol-type   = zcl_ctxfreegram=>lcs_symbol_type-terminal.
      ls_symbol-index  = au_terminal_end_of_input.
      COLLECT ls_symbol INTO rs_follow_set-t_follow_symbol.
      RETURN.
    ENDIF.

    " anti-recursive
    lt_preceding_symbol = it_preceding_symbol.
    INSERT is_symbol INTO TABLE lt_preceding_symbol.

    LOOP AT aut_rule ASSIGNING <ls_rule>.
      " for all terminals and nonterminals of RHS, except for symbol "end of input", the FOLLOW SET must be determined
      LOOP AT <ls_rule>-t_symbol ASSIGNING <ls_symbol>
            WHERE type = is_symbol-type
              AND index = is_symbol-index.
        IF sy-tabix = lines( <ls_rule>-t_symbol ).
          " if symbol is at the end of RHS, then take the follow-set of LHS of the production rule
          ls_symbol-type   = zcl_ctxfreegram=>lcs_symbol_type-nonterminal.
          ls_symbol-index  = <ls_rule>-lhs_nonterm_index.
          " anti-recursive
          READ TABLE lt_preceding_symbol WITH KEY type = ls_symbol-type
                                                  index = ls_symbol-index
                TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            ls_follow_set = create_follow_set2( is_symbol = ls_symbol
                                                it_preceding_symbol = lt_preceding_symbol ).
            LOOP AT ls_follow_set-t_follow_symbol ASSIGNING <ls_symbol2>.
              COLLECT <ls_symbol2> INTO rs_follow_set-t_follow_symbol.
            ENDLOOP.
          ENDIF.
        ELSE.
          " if symbol is NOT at the end of RHS, then take the first-set of the next symbol
          l_tabix = sy-tabix + 1.
          READ TABLE <ls_rule>-t_symbol INDEX l_tabix ASSIGNING <ls_symbol2>.
          ASSERT sy-subrc = 0.
          IF <ls_symbol2>-type = zcl_ctxfreegram=>lcs_symbol_type-terminal.
            COLLECT <ls_symbol2> INTO rs_follow_set-t_follow_symbol.
          ELSE.
            LOOP AT aut_first_set ASSIGNING <ls_first_set>
                  WHERE lhs_nonterm_index = <ls_symbol2>-index.
              COLLECT <ls_first_set>-s_rhs INTO rs_follow_set-t_follow_symbol.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_goto_table.

    aut_goto = VALUE #(
        FOR <ls_transition> IN aut_transition
        WHERE ( s_symbol-type = lcs_symbol_type-nonterminal )
        ( state             = <ls_transition>-itemset_index
          lhs_nonterm_index = <ls_transition>-s_symbol-index
          next_state        = <ls_transition>-goto_itemset_index ) ).

  ENDMETHOD.


  METHOD create_item_set_transit_tables.
    DATA: lt_closure      TYPE TABLE OF sytabix, "list of all closed nonterminals
          l_goto_itemset  TYPE sytabix,
          ls_item_new     TYPE ty_us_item,
          lt_symbol       TYPE zcl_ctxfreegram=>ty_ut_symbol,
          l_already_found TYPE flag.
    FIELD-SYMBOLS:
      <ls_itemset>     TYPE ty_us_itemset,
      <ls_item>        TYPE ty_us_item,
      <ls_rule>        TYPE zcl_ctxfreegram=>ty_us_rule,
      <ls_symbol>      TYPE zcl_ctxfreegram=>ty_us_symbol,
      <ls_item_new>    TYPE ty_us_item,
      <ls_itemset_new> TYPE ty_us_itemset,
      <ls_itemset_bis> TYPE ty_us_itemset,
      <ls_item_bis>    TYPE ty_us_item,
      <ls_symbol_new>  TYPE zcl_ctxfreegram=>ty_us_symbol,
      <ls_transition>  TYPE ty_us_transition.

*    REFRESH aut_itemset.
    REFRESH aut_transition.

    " Create ITEMSET with only ITEMSET(1)
    aut_itemset = VALUE #(
        LET <aux_rule> = aut_rule[ au_starting_nonterminal ] IN
        ( index        = 1
          start_symbol = aut_symbol[ type = lcs_symbol_type-nonterminal value = au_start_rule->name ]
          t_item       = VALUE #(
                        ( rule_index        = <aux_rule>-index
                          type              = lcs_item_type-kernel
                          dot_index         = 1
                          s_dot_symbol      = VALUE #( <aux_rule>-t_symbol[ 1 ] OPTIONAL )
                          lhs_nonterm_index = <aux_rule>-lhs_nonterm_index
                          nr_of_rhs_symbols = lines( <aux_rule>-t_symbol )
                          dot_at_the_end    = xsdbool( <aux_rule>-t_symbol IS INITIAL ) ) ) ) ).

*    APPEND INITIAL LINE TO aut_itemset ASSIGNING <ls_itemset>.
*    <ls_itemset>-index = sy-tabix.
*    READ TABLE aut_rule INDEX 1 ASSIGNING <ls_rule>.
*    ASSERT sy-subrc = 0.
*    <ls_itemset>-start_symbol = aut_symbol[ type = lcs_symbol_type-nonterminal value = au_start_rule->name ].
*    APPEND INITIAL LINE TO <ls_itemset>-t_item ASSIGNING <ls_item>.
*    <ls_item>-rule_index = <ls_rule>-index.
*    <ls_item>-type = lcs_item_type-kernel.
*    READ TABLE <ls_rule>-t_symbol ASSIGNING <ls_symbol> INDEX 1.
*    IF sy-subrc = 0.
*      <ls_item>-dot_index = sy-tabix.
*      <ls_item>-s_dot_symbol = <ls_symbol>.
*    ENDIF.
*    <ls_item>-lhs_nonterm_index = <ls_rule>-lhs_nonterm_index.
*    <ls_item>-nr_of_rhs_symbols = lines( <ls_rule>-t_symbol ).
*    IF <ls_item>-dot_index > <ls_item>-nr_of_rhs_symbols.
*      <ls_item>-dot_at_the_end = abap_true.
*    ELSE.
*      <ls_item>-dot_at_the_end = abap_false.
*    ENDIF.

    " Process all item sets.
    " At the beginning, there's only one item set
    " But this loop adds new item sets.
    LOOP AT aut_itemset ASSIGNING <ls_itemset>.

      " Do the "closure"
      REFRESH lt_closure.
      LOOP AT <ls_itemset>-t_item ASSIGNING <ls_item>
            WHERE s_dot_symbol-type = zcl_ctxfreegram=>lcs_symbol_type-nonterminal.
        " Process each nonterminal only once
        READ TABLE lt_closure WITH KEY table_line = <ls_item>-s_dot_symbol-index
              TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND <ls_item>-s_dot_symbol-index TO lt_closure.
          LOOP AT aut_rule ASSIGNING <ls_rule>
                WHERE lhs_nonterm_index = <ls_item>-s_dot_symbol-index.
            APPEND INITIAL LINE TO <ls_itemset>-t_item ASSIGNING <ls_item_new>.
            <ls_item_new>-rule_index = <ls_rule>-index.
            <ls_item_new>-type = lcs_item_type-closure.
            READ TABLE <ls_rule>-t_symbol ASSIGNING <ls_symbol> INDEX 1.
            " if there's no terminal nor nonterminal (i.e. epsilon)
            IF sy-subrc <> 0.
              <ls_item_new>-dot_index = 1.
            ELSE.
              <ls_item_new>-dot_index = 1.
              <ls_item_new>-s_dot_symbol = <ls_symbol>.
            ENDIF.
            <ls_item_new>-lhs_nonterm_index = <ls_rule>-lhs_nonterm_index.
            <ls_item_new>-nr_of_rhs_symbols = lines( <ls_rule>-t_symbol ).
            IF <ls_item_new>-dot_index > <ls_item_new>-nr_of_rhs_symbols.
              <ls_item_new>-dot_at_the_end = abap_true.
            ELSE.
              <ls_item_new>-dot_at_the_end = abap_false.
            ENDIF.
            "
            IF <ls_item_new>-dot_at_the_end = abap_true.
              <ls_itemset>-index_of_reduced_rule = <ls_item_new>-rule_index.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
*      FREE lt_closure.

      " list of symbols of current item set, positioned on DOT_INDEX
      REFRESH lt_symbol.
      LOOP AT <ls_itemset>-t_item ASSIGNING <ls_item>
            WHERE s_dot_symbol IS NOT INITIAL.
        COLLECT <ls_item>-s_dot_symbol INTO lt_symbol.
      ENDLOOP.
      " sort to place the terminals before nonterminals
      SORT lt_symbol BY type DESCENDING index.
      " for each symbol
      LOOP AT lt_symbol ASSIGNING <ls_symbol>.
        UNASSIGN <ls_itemset_new>.
        CLEAR l_goto_itemset.
        " for each item which contains this symbol
        LOOP AT <ls_itemset>-t_item ASSIGNING <ls_item> WHERE s_dot_symbol = <ls_symbol>.
          " create an item set with these items
          CLEAR ls_item_new.
          ls_item_new-type = lcs_item_type-kernel.
          ls_item_new-rule_index = <ls_item>-rule_index.
          " determine next dot index
          ls_item_new-dot_index = <ls_item>-dot_index + 1.
          READ TABLE aut_rule
                INDEX <ls_item>-rule_index ASSIGNING <ls_rule>.
          IF sy-subrc <> 0.
            error( ).
          ENDIF.
          READ TABLE <ls_rule>-t_symbol INDEX ls_item_new-dot_index ASSIGNING <ls_symbol_new>.
          IF sy-subrc = 0.
            ls_item_new-s_dot_symbol = <ls_symbol_new>.
          ELSE.
            " last symbol of production rule -> do nothing
            CLEAR ls_item_new-s_dot_symbol.
          ENDIF.
          ls_item_new-lhs_nonterm_index = <ls_rule>-lhs_nonterm_index.
          ls_item_new-nr_of_rhs_symbols = lines( <ls_rule>-t_symbol ).
          IF ls_item_new-dot_index > ls_item_new-nr_of_rhs_symbols.
            ls_item_new-dot_at_the_end = abap_true.
          ELSE.
            ls_item_new-dot_at_the_end = abap_false.
          ENDIF.
          " determine whether item set already exists: no -> create it
          l_already_found = ''.
          LOOP AT aut_itemset ASSIGNING <ls_itemset_bis>.
            LOOP AT <ls_itemset_bis>-t_item ASSIGNING <ls_item_bis>
                WHERE dot_index  = ls_item_new-dot_index
                  AND rule_index = ls_item_new-rule_index.
              l_already_found = 'X'.
              EXIT.
            ENDLOOP.
            IF l_already_found = 'X'.
              EXIT.
            ENDIF.
          ENDLOOP.
          IF l_already_found = 'X'.
            l_goto_itemset = <ls_itemset_bis>-index.
          ELSE.
            IF <ls_itemset_new> IS NOT ASSIGNED.
              APPEND INITIAL LINE TO aut_itemset ASSIGNING <ls_itemset_new>.
              <ls_itemset_new>-index        = sy-tabix.
              <ls_itemset_new>-start_symbol = <ls_symbol>.
              <ls_itemset_new>-rule_index   = ls_item_new-rule_index.
              <ls_itemset_new>-rule_lhs     = <ls_rule>-lhs_nonterm_index.
            ENDIF.
            APPEND ls_item_new TO <ls_itemset_new>-t_item.
            "
            IF ls_item_new-dot_at_the_end = abap_true.
              <ls_itemset_new>-index_of_reduced_rule = ls_item_new-rule_index.
            ENDIF.
            "
            l_goto_itemset = <ls_itemset_new>-index.
          ENDIF.
        ENDLOOP.
        ASSERT sy-subrc = 0.
        " add the transition of the starting item to the target item set
        APPEND INITIAL LINE TO aut_transition ASSIGNING <ls_transition>.
        <ls_transition>-itemset_index       = <ls_itemset>-index.
        <ls_transition>-s_symbol            = <ls_symbol>.
        <ls_transition>-goto_itemset_index  = l_goto_itemset.
      ENDLOOP.
      ASSERT 1 = 1.
    ENDLOOP.
  ENDMETHOD.


  METHOD error.
    BREAK-POINT.
*  ENDMETHOD.
*
*  METHOD add_rule.
*    DATA l_arg2 TYPE string.
*    FIELD-SYMBOLS:
*      <ls_rule>        TYPE ty_us_rule,
*      <ls_nonterminal> TYPE ty_us_nonterminal,
*      <ls_terminal>    TYPE ty_us_terminal,
*      <ls_symbol>      TYPE ty_us_symbol,
*      <ls_symbol2>     TYPE ty_us_symbol2.
*
*    ASSIGN ai_ref_rule->* TO <ls_rule>.
*    l_arg2 = i_next_symbol.
*    IF l_arg2 = l_end_of_rule_symbol.
*      IF <ls_rule>-t_symbol IS INITIAL.
*        APPEND INITIAL LINE TO <ls_rule>-t_symbol ASSIGNING <ls_symbol>.
*        <ls_symbol>-index = 1.
*        <ls_symbol>-type = lcs_symbol_type-terminal.
*      ENDIF.
*    ELSE.
*      APPEND INITIAL LINE TO <ls_rule>-t_symbol ASSIGNING <ls_symbol>.
*      IF l_arg2(1) = `"`.
*        <ls_symbol>-type = lcs_symbol_type-terminal.
*        FIND REGEX `^"(.*)"$` IN l_arg2 SUBMATCHES l_arg2.
*        <ls_symbol>-index = add_terminal( l_arg2 ).
**        READ TABLE aut_terminal WITH KEY terminal = l_arg2 ASSIGNING <ls_terminal>.
**        IF sy-subrc <> 0.
**          APPEND INITIAL LINE TO aut_terminal ASSIGNING <ls_terminal>.
**          <ls_terminal>-index   = sy-tabix.
**          <ls_terminal>-terminal = l_arg2.
**          APPEND INITIAL LINE TO aut_symbol ASSIGNING <ls_symbol2>.
**          <ls_symbol2>-type  = lcs_symbol_type-terminal.
**          <ls_symbol2>-index = <ls_terminal>-index.
**          <ls_symbol2>-value = <ls_terminal>-terminal.
**        ENDIF.
**        <ls_symbol>-index = <ls_terminal>-index.
*      ELSE.
*        <ls_symbol>-type = lcs_symbol_type-nonterminal.
*        <ls_symbol>-index = add_nonterminal( l_arg2 ).
**        READ TABLE aut_nonterminal WITH KEY nonterminal = l_arg2 ASSIGNING <ls_nonterminal>.
**        IF sy-subrc <> 0.
**          APPEND INITIAL LINE TO aut_nonterminal ASSIGNING <ls_nonterminal>.
**          <ls_nonterminal>-index      = sy-tabix.
**          <ls_nonterminal>-nonterminal = l_arg2.
**          APPEND INITIAL LINE TO aut_symbol ASSIGNING <ls_symbol2>.
**          <ls_symbol2>-type  = lcs_symbol_type-nonterminal.
**          <ls_symbol2>-index = <ls_nonterminal>-index.
**          <ls_symbol2>-value = <ls_nonterminal>-nonterminal.
**        ENDIF.
**        <ls_symbol>-index = <ls_nonterminal>-index.
*      ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD get_freetext.
    FIELD-SYMBOLS <ls_freetext> TYPE zcl_ctxfreegram=>ty_us_freetext.
    READ TABLE aut_freetext
          WITH KEY index = i_index
          ASSIGNING <ls_freetext>.
    ASSERT sy-subrc = 0.
    r_result = <ls_freetext>-value.
  ENDMETHOD.


  METHOD get_nonterminal.
    FIELD-SYMBOLS <ls_nonterminal> TYPE zcl_ctxfreegram=>ty_us_nonterminal.
    READ TABLE aut_nonterminal
          WITH KEY index = i_index
          ASSIGNING <ls_nonterminal>.
    ASSERT sy-subrc = 0.
    r_result = <ls_nonterminal>-nonterminal.
  ENDMETHOD.


  METHOD get_rules_as_text.
    DATA: l_rule                       TYPE string,
          l_string                     TYPE string,
          l_previous_symbol_is_t_or_nt TYPE abap_bool.
    FIELD-SYMBOLS:
      <ls_rule>   TYPE ty_us_rule,
      <ls_symbol> TYPE zcl_ctxfreegram=>ty_us_symbol.

    REFRESH et_text.
    CLEAR e_text.
    REFRESH et_stream.
    LOOP AT aut_rule ASSIGNING <ls_rule>.
      CLEAR l_rule.
      l_rule = get_nonterminal( <ls_rule>-lhs_nonterm_index ).
      CONCATENATE l_rule ` (*` <ls_rule>-name_alv `*) = ` INTO l_rule.
      l_previous_symbol_is_t_or_nt = abap_false.
      LOOP AT <ls_rule>-t_symbol_including_comments ASSIGNING <ls_symbol>.
        l_string = get_symbol( <ls_symbol> ).
        IF l_previous_symbol_is_t_or_nt = abap_true
              AND ( <ls_symbol>-type = lcs_symbol_type-terminal
                OR <ls_symbol>-type = lcs_symbol_type-nonterminal ).
          CONCATENATE l_rule `, ` l_string INTO l_rule.
          l_previous_symbol_is_t_or_nt = abap_false.
        ELSE.
          CONCATENATE l_rule ` ` l_string INTO l_rule.
        ENDIF.
        IF <ls_symbol>-type = lcs_symbol_type-terminal
              OR <ls_symbol>-type = lcs_symbol_type-nonterminal.
          l_previous_symbol_is_t_or_nt = abap_true.
        ENDIF.
      ENDLOOP.
      CONCATENATE l_rule ` ;` INTO l_rule.
      IF et_text IS SUPPLIED.
        APPEND l_rule TO et_text.
      ENDIF.
      IF e_text IS SUPPLIED OR et_stream IS SUPPLIED.
        CONCATENATE e_text cl_abap_char_utilities=>cr_lf l_rule INTO e_text.
      ENDIF.
    ENDLOOP.
    IF et_stream IS SUPPLIED.
      CALL FUNCTION 'SWA_STRING_TO_TABLE'
        EXPORTING
          character_string = e_text
        IMPORTING
          character_table  = et_stream.
    ENDIF.
  ENDMETHOD.


  METHOD get_symbol.
    IF is_symbol-type = lcs_symbol_type-nonterminal.
      r_result = get_nonterminal( is_symbol-index ).
    ELSEIF is_symbol-type = lcs_symbol_type-terminal.
      r_result = get_terminal( is_symbol-index ).
    ELSEIF is_symbol-type = lcs_symbol_type-comment.
      r_result = get_freetext( is_symbol-index ).
    ENDIF.
  ENDMETHOD.


  METHOD get_terminal.
    FIELD-SYMBOLS <ls_terminal> TYPE zcl_ctxfreegram=>ty_us_terminal.
    READ TABLE aut_terminal
          WITH KEY index = i_index
          ASSIGNING <ls_terminal>.
    ASSERT sy-subrc = 0.
    IF i_abap = abap_false.
      CONCATENATE '"' <ls_terminal>-terminal '"' INTO r_result.
    ELSE.
      r_result = <ls_terminal>-terminal.
    ENDIF.
  ENDMETHOD.


  METHOD remove_useless_reduce_actions.
    DATA: ls_symbol TYPE zcl_ctxfreegram=>ty_us_symbol.
    FIELD-SYMBOLS:
      <ls_item>       TYPE ty_us_item,
      <ls_itemset>    TYPE ty_us_itemset,
      <ls_action>     TYPE ty_us_action,
      <ls_follow_set> TYPE ty_us_follow_set.

    LOOP AT aut_action ASSIGNING <ls_action>
          WHERE action = lcs_action-reduce.
      READ TABLE aut_itemset WITH KEY index = <ls_action>-state ASSIGNING <ls_itemset>.
      ASSERT sy-subrc = 0.
      IF <ls_itemset>-rule_lhs = 0.
        " delete all REDUCE of the starting rule, except for symbol "end of input"
        IF <ls_action>-terminal_index <> au_terminal_end_of_input.
          DELETE aut_action.
        ENDIF.
      ELSE.
        " delete REDUCE if none of follow set of current symbols of item set doesn't contain the terminal of the action.
        DATA do_delete TYPE abap_bool.
        do_delete = abap_true.
        LOOP AT <ls_itemset>-t_item ASSIGNING <ls_item>
              WHERE type = lcs_item_type-kernel.
          READ TABLE aut_follow_set WITH KEY symbol = <ls_item>-s_dot_symbol ASSIGNING <ls_follow_set>.
          ASSERT sy-subrc = 0.
          CLEAR ls_symbol.
          ls_symbol-type  = zcl_ctxfreegram=>lcs_symbol_type-terminal.
          ls_symbol-index = <ls_action>-terminal_index.
          READ TABLE <ls_follow_set>-t_follow_symbol WITH KEY table_line = ls_symbol
                TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            do_delete = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF do_delete = abap_true.
          DELETE aut_action.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD render_itemsets.

    result = concat_lines_of( table = VALUE string_table(
        FOR <itemset> IN aut_itemset INDEX INTO i
        ( |* Item set { i }:\n| )
        ( LINES OF VALUE #(
            FOR <item> IN <itemset>-t_item
            WHERE ( nr_of_rhs_symbols > 0 )
            LET rule = aut_rule[ <item>-rule_index ] IN
            ( |** Item { COND #( WHEN <item>-type = lcs_item_type-kernel THEN `kernel` ELSE `closure` ) }: { rule-lhs->get_text( ) } = {
                COND #( WHEN rule-rhs IS BOUND THEN
                concat_lines_of( sep = ` ` table = VALUE string_table(
                FOR elem IN rule-rhs->get_elements( ) INDEX INTO j
                ( COND #( WHEN j = <item>-dot_index THEN `◆ ` ) && elem->get_text( ) && COND #( WHEN <item>-dot_at_the_end = abap_true AND j = <item>-nr_of_rhs_symbols THEN ` ◆` ) ) ) ) ) }\n| ) ) ) ) ).

  ENDMETHOD.

  METHOD render_transitions.

    DATA ref_table TYPE REF TO data.
    FIELD-SYMBOLS <transitions> TYPE STANDARD TABLE.

    ref_table = create_render_table( ).
    ASSIGN ref_table->* TO <transitions>.

    " HEADING
    APPEND INITIAL LINE TO <transitions> ASSIGNING FIELD-SYMBOL(<transition>).
    LOOP AT aut_symbol ASSIGNING FIELD-SYMBOL(<symbol>).
      DATA(compname) = |{ COND #( WHEN <symbol>-type = lcs_symbol_type-terminal THEN 'T' ELSE 'N' ) }{ <symbol>-index }|.
      ASSIGN COMPONENT compname OF STRUCTURE <transition> TO FIELD-SYMBOL(<comp>).
      <comp> = <symbol>-object->get_text( ).
    ENDLOOP.

    " TRANSITIONS
    LOOP AT aut_itemset REFERENCE INTO DATA(itemset).

      APPEND INITIAL LINE TO <transitions> ASSIGNING <transition>.

      ASSIGN COMPONENT 'ITEMSET' OF STRUCTURE <transition> TO <comp>.
      <comp> = |{ itemset->index }|.

      LOOP AT aut_transition REFERENCE INTO DATA(transition)
          WHERE itemset_index = itemset->index.
        compname = |{ COND #( WHEN transition->s_symbol-type = lcs_symbol_type-terminal THEN 'T' ELSE 'N' ) }{ transition->s_symbol-index }|.
        ASSIGN COMPONENT compname OF STRUCTURE <transition> TO <comp>.
        IF sy-subrc <> 0.
          error( ).
        ENDIF.
        <comp> = transition->goto_itemset_index.
      ENDLOOP.

    ENDLOOP.

    result = render_table( <transitions> ).

  ENDMETHOD.

  METHOD render_rules.

    result = REDUCE #(
        INIT t = ``
        FOR rule IN aut_rule
        NEXT t = t && |{ rule-lhs->get_text( ) }: { rule-rhs->get_text( ) }\n| ).

  ENDMETHOD.

  METHOD render_action_goto_tables.

    DATA ref_table TYPE REF TO data.
    FIELD-SYMBOLS <actions> TYPE STANDARD TABLE.

    ref_table = create_render_table( ).
    ASSIGN ref_table->* TO <actions>.

    " HEADING
    APPEND INITIAL LINE TO <actions> ASSIGNING FIELD-SYMBOL(<action>).
    LOOP AT aut_symbol ASSIGNING FIELD-SYMBOL(<symbol>).
      DATA(compname) = |{ COND #( WHEN <symbol>-type = lcs_symbol_type-terminal THEN 'T' ELSE 'N' ) }{ <symbol>-index }|.
      ASSIGN COMPONENT compname OF STRUCTURE <action> TO FIELD-SYMBOL(<comp>).
      <comp> = <symbol>-object->get_text( ).
    ENDLOOP.

    " ACTIONS
    LOOP AT aut_itemset REFERENCE INTO DATA(itemset).

      APPEND INITIAL LINE TO <actions> ASSIGNING <action>.

      ASSIGN COMPONENT 'ITEMSET' OF STRUCTURE <action> TO <comp>.
      <comp> = |{ itemset->index }|.

      LOOP AT aut_action REFERENCE INTO DATA(action)
          WHERE state = itemset->index.
        compname = |T{ action->terminal_index }|.
        ASSIGN COMPONENT compname OF STRUCTURE <action> TO <comp>.
        IF sy-subrc <> 0.
          error( ).
        ENDIF.
        <comp> = COND string( WHEN action->action = 'a' THEN |acc| ELSE |{ action->action }{ action->index }| ).
      ENDLOOP.

      LOOP AT aut_transition REFERENCE INTO DATA(transition)
          WHERE s_symbol-type = lcs_symbol_type-nonterminal
            AND itemset_index = itemset->index.
        compname = |N{ transition->s_symbol-index }|.
        ASSIGN COMPONENT compname OF STRUCTURE <action> TO <comp>.
        IF sy-subrc <> 0.
          error( ).
        ENDIF.
        <comp> = transition->goto_itemset_index. "cond string( when action->action = 'a' then |acc| else |{ action->action }{ action->index }| ).
      ENDLOOP.

    ENDLOOP.

    result = render_table( <actions> ).

  ENDMETHOD.

  METHOD create_render_table.

    DATA(components) = VALUE abap_component_tab(
            LET type = cl_abap_elemdescr=>get_string( ) IN
        ( name = 'ITEMSET' type = cl_abap_elemdescr=>get_string( ) )
        ( LINES OF COND #( WHEN terminals = abap_true THEN VALUE #(
            FOR <terminal> IN aut_terminal
            ( name = 'T' && <terminal>-index
              type = type ) ) ) )
        ( LINES OF COND #( WHEN nonterminals = abap_true THEN VALUE #(
            FOR <nonterminal> IN aut_nonterminal
            ( name = 'N' && <nonterminal>-index
              type = type ) ) ) ) ).

    DATA(rtti_table) = cl_abap_tabledescr=>get( p_line_type = cl_abap_structdescr=>get( components ) ).
    CREATE DATA result TYPE HANDLE rtti_table.

  ENDMETHOD.

  METHOD render_table.

    TYPES ty_column_widths TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    DATA(components) = CAST cl_abap_structdescr( CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( table ) )->get_table_line_type( ) )->get_components( ).
    DATA(column_widths) = VALUE ty_column_widths( ).
    DO lines( components ) TIMES.
      DATA(max_width) = 0.
      LOOP AT table ASSIGNING FIELD-SYMBOL(<line>).
        ASSIGN COMPONENT sy-index OF STRUCTURE <line> TO FIELD-SYMBOL(<comp>).
        max_width = nmax( val1 = max_width val2 = strlen( |{ <comp> }| ) ).
      ENDLOOP.
      max_width = max_width + 2. " number of blanks between columns
      APPEND max_width TO column_widths.
    ENDDO.

    LOOP AT table ASSIGNING <line>.
      DATA(line) = ``.
      DO lines( column_widths ) TIMES.
        ASSIGN COMPONENT sy-index OF STRUCTURE <line> TO <comp>.
        line = line && |{ <comp> WIDTH = column_widths[ sy-index ] }|.
      ENDDO.
      result = result && line && |\n|.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
