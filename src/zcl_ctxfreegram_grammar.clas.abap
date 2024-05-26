CLASS zcl_ctxfreegram_grammar DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES ty_u_symbol_type TYPE c LENGTH 1.
    TYPES:
      BEGIN OF ty_us_symbol,
        type   TYPE ty_u_symbol_type,
        index  TYPE sytabix,
        value  TYPE string,
        object TYPE REF TO zif_ctxfreegram_rule_elem,
      END OF ty_us_symbol.
    TYPES ty_ut_symbol TYPE STANDARD TABLE OF ty_us_symbol WITH NON-UNIQUE KEY type index value object.
    TYPES:
      BEGIN OF ty_us_rule,
        lhs                         TYPE REF TO zif_ctxfreegram_rule_nonterm,
        rhs                         TYPE REF TO zif_ctxfreegram_rule_elem,
        index                       TYPE sytabix,
        id                          TYPE string,
        lhs_nonterm_index           TYPE sytabix,
        name_alv                    TYPE string,
        plain_text                  TYPE string,
        "! symbols except comments and special instructions
        t_symbol                    TYPE ty_ut_symbol,
        t_symbol_including_comments TYPE ty_ut_symbol,
      END OF ty_us_rule.
    TYPES tt_rule        TYPE STANDARD TABLE OF ty_us_rule WITH EMPTY KEY.
    TYPES ty_u_item_type TYPE c LENGTH 1.
    TYPES:
      BEGIN OF ty_us_item,
        rule_index        TYPE sytabix,
        lhs_nonterm_index TYPE sytabix,
        type              TYPE ty_u_item_type,
        dot_index         TYPE sytabix,
        dot_at_the_end    TYPE abap_bool,
        nr_of_rhs_symbols TYPE i,
        s_dot_symbol      TYPE ty_us_symbol,
      END OF ty_us_item.
    TYPES ty_ut_item TYPE STANDARD TABLE OF ty_us_item WITH DEFAULT KEY.
    TYPES:
      BEGIN OF ty_us_item_set,
        index                 TYPE sytabix,
        start_symbol          TYPE ty_us_symbol,
        items                 TYPE ty_ut_item,
        "! ??? (weird as several rules may correspond to one item set)
        rule_index            TYPE sytabix,
        "! ???
        index_of_reduced_rule TYPE sytabix,
      END OF ty_us_item_set.
    TYPES tt_item_set TYPE STANDARD TABLE OF ty_us_item_set WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_us_transition,
        item_set_index      TYPE sytabix,
        s_symbol            TYPE ty_us_symbol,
        goto_item_set_index TYPE sytabix,
      END OF ty_us_transition.
    TYPES tt_transition TYPE STANDARD TABLE OF ty_us_transition WITH EMPTY KEY.
    "! s = shift, r = reduce, a = accept
    "! (values in constant LCS_ACTION)
    TYPES ty_u_action TYPE c LENGTH 1.
    TYPES:
      BEGIN OF ty_us_action,
        "! State / Item set number
        state          TYPE i,
        "! Terminal number
        terminal_index TYPE sytabix,
        "! Shift, Reduce or Accept
        action         TYPE ty_u_action,
        "! If shift, it's the new state/item set, if reduce it's the rule number
        index          TYPE sytabix,
      END OF ty_us_action.
    TYPES tt_action TYPE SORTED TABLE OF ty_us_action WITH UNIQUE KEY state terminal_index.
    TYPES:
      BEGIN OF ty_us_goto,
        "! State / Item set number
        state             TYPE sytabix,
        "! nonterminal
        lhs_nonterm_index TYPE sytabix,
        "! Next State / Item set number after reducing nonterminal at given state
        next_state        TYPE sytabix,
      END OF ty_us_goto.
    TYPES ty_goto_table TYPE HASHED TABLE OF ty_us_goto WITH UNIQUE KEY state lhs_nonterm_index.
    TYPES:
      BEGIN OF ty_us_first_set,
        lhs_nonterm_index TYPE sytabix,
        s_rhs             TYPE zcl_ctxfreegram_grammar=>ty_us_symbol,
      END OF ty_us_first_set.
    TYPES ty_ut_first_set TYPE STANDARD TABLE OF ty_us_first_set WITH NON-UNIQUE KEY lhs_nonterm_index s_rhs.
    TYPES:
      BEGIN OF ty_us_follow_set,
        symbol          TYPE zcl_ctxfreegram_grammar=>ty_us_symbol,
        t_follow_symbol TYPE zcl_ctxfreegram_grammar=>ty_ut_symbol,
      END OF ty_us_follow_set.
    TYPES tt_follow_set  TYPE STANDARD TABLE OF ty_us_follow_set WITH NON-UNIQUE KEY symbol.
    TYPES:
      BEGIN OF ty_rule,
        lhs TYPE REF TO zif_ctxfreegram_rule_nonterm,
        rhs TYPE REF TO zif_ctxfreegram_rule_elem,
      END OF ty_rule.
    TYPES ty_rules TYPE STANDARD TABLE OF ty_rule WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_us_terminal,
        index    TYPE sytabix,
        terminal TYPE string,
        object   TYPE REF TO zif_ctxfreegram_rule_elem,
      END OF ty_us_terminal.
    TYPES tt_terminal    TYPE STANDARD TABLE OF ty_us_terminal WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_us_nonterminal,
        index       TYPE sytabix,
        nonterminal TYPE string,
        object      TYPE REF TO zif_ctxfreegram_rule_elem,
      END OF ty_us_nonterminal.
    TYPES tt_nonterminal TYPE STANDARD TABLE OF ty_us_nonterminal WITH EMPTY KEY.

    CONSTANTS:
      BEGIN OF lcs_item_type,
        kernel  TYPE ty_u_item_type VALUE 'K',
        closure TYPE ty_u_item_type VALUE 'C',
      END OF lcs_item_type.
    CONSTANTS:
      BEGIN OF lcs_action,
        shift  TYPE ty_u_action VALUE 's',
        reduce TYPE ty_u_action VALUE 'r',
        accept TYPE ty_u_action VALUE 'a',
      END OF lcs_action.

    DATA au_start_rule           TYPE REF TO zif_ctxfreegram_rule_nonterm READ-ONLY.
    DATA formatted_rules         TYPE tt_rule                             READ-ONLY.
    DATA aut_item_set            TYPE tt_item_set                         READ-ONLY.
    DATA aut_transition          TYPE tt_transition                       READ-ONLY.
    DATA aut_action              TYPE tt_action                           READ-ONLY.
    DATA aut_goto                TYPE ty_goto_table                       READ-ONLY.
    "! List of all possible terminals for each nonterminal, which may occur at the start of this nonterminal
    DATA aut_first_set           TYPE ty_ut_first_set                     READ-ONLY.
    "! List of all possible terminals for each nonterminal, which may occur after this nonterminal
    DATA aut_follow_set          TYPE tt_follow_set                       READ-ONLY.
    DATA symbol_end_of_input     TYPE ty_us_symbol                        READ-ONLY.
    DATA au_starting_nonterminal TYPE sytabix                             READ-ONLY.
    DATA aut_nonterminal         TYPE tt_nonterminal                      READ-ONLY.
    DATA aut_terminal            TYPE tt_terminal                         READ-ONLY.

    CLASS-METHODS create
      IMPORTING
        start_rule    TYPE REF TO zif_ctxfreegram_rule_nonterm
        rules         TYPE ty_rules
      RETURNING
        VALUE(result) TYPE REF TO zcl_ctxfreegram_grammar
      RAISING
        zcx_ctxfreegram.

    CLASS-METHODS create_from_string_table
      IMPORTING
        start_rule    TYPE csequence
        string_table  TYPE string_table
      RETURNING
        VALUE(result) TYPE REF TO zcl_ctxfreegram_grammar
      RAISING
        zcx_ctxfreegram.

    METHODS render_action_goto_table
      RETURNING
        VALUE(result) TYPE string.

    METHODS render_first_sets
      RETURNING
        VALUE(result) TYPE string.

    METHODS render_follow_sets
      RETURNING
        VALUE(result) TYPE string.

    METHODS render_item_sets
      RETURNING
        VALUE(result) TYPE string.

    METHODS render_rules
      RETURNING
        VALUE(result) TYPE string.

    METHODS render_transitions
      RETURNING
        VALUE(result) TYPE string.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_us_freetext,
        index TYPE sytabix,
        value TYPE string,
      END OF ty_us_freetext .
    TYPES:
      BEGIN OF ty_ls_lhs,
        nonterminal_index TYPE i,
        count             TYPE i,
      END OF ty_ls_lhs .
    TYPES ty_i_set_type TYPE i .
    TYPES:
      BEGIN OF ty_is_request,
        type   TYPE ty_i_set_type,
        symbol TYPE zcl_ctxfreegram_grammar=>ty_us_symbol,
      END OF ty_is_request .
    TYPES:
      ty_it_request     TYPE TABLE OF ty_is_request .
    TYPES ty_i_nonterminal TYPE sytabix .
    TYPES:
      ty_it_nonterminal TYPE TABLE OF sytabix,
      ty_column_widths  TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    CONSTANTS:
      BEGIN OF lcs_symbol_type,
        nonterminal      TYPE ty_u_symbol_type VALUE 'N',
        terminal         TYPE ty_u_symbol_type VALUE 'T',
        terminal_regex   TYPE ty_u_symbol_type VALUE 'R',
        comment          TYPE ty_u_symbol_type VALUE '*',
        special_sequence TYPE ty_u_symbol_type VALUE '?',
        empty            TYPE ty_u_symbol_type VALUE 'E',
      END OF lcs_symbol_type .
    CONSTANTS:
      BEGIN OF c_is_set_type,
        first_set  TYPE ty_i_set_type VALUE 1,
        follow_set TYPE ty_i_set_type VALUE 2,
      END OF c_is_set_type .
    DATA transmitted_rules TYPE ty_rules .
    DATA:
      aut_symbol                TYPE TABLE OF ty_us_symbol .
    DATA:
      aut_freetext              TYPE TABLE OF ty_us_freetext .
    DATA auo_terminal_end_of_input TYPE REF TO zif_ctxfreegram_rule_term .

    METHODS add_nonterminal
      IMPORTING
        !nonterminal TYPE REF TO zif_ctxfreegram_rule_nonterm
      RETURNING
        VALUE(index) TYPE ty_us_symbol .
    METHODS add_terminal
      IMPORTING
        !terminal    TYPE REF TO zif_ctxfreegram_rule_term
      RETURNING
        VALUE(index) TYPE ty_us_symbol .
    METHODS add_terminal_regex
      IMPORTING
        !terminal_regex TYPE REF TO zif_ctxfreegram_rule_termregex
      RETURNING
        VALUE(index)    TYPE ty_us_symbol .
    METHODS check_lhs_rhs_is_bound
      RAISING
        zcx_ctxfreegram .
    METHODS check_rhs_nonterminals_in_lhs
      RAISING
        zcx_ctxfreegram .
    METHODS check_start_rule_appears_once
      RAISING
        zcx_ctxfreegram .
    METHODS check_validity
      RAISING
        zcx_ctxfreegram .
    METHODS create_2
      RAISING
        zcx_ctxfreegram .
    METHODS create_action_table .
    METHODS create_first_and_follow_sets .
    METHODS create_first_or_follow_set
      IMPORTING
        !is_request           TYPE ty_is_request
        !it_processed_request TYPE ty_it_request OPTIONAL
      RETURNING
        VALUE(rt_symbol)      TYPE zcl_ctxfreegram_grammar=>ty_ut_symbol .
    METHODS create_first_set .
    METHODS create_first_set2
      IMPORTING
        !i_nonterminal             TYPE ty_i_nonterminal
        !it_preceding_nonterminals TYPE ty_it_nonterminal OPTIONAL
      RETURNING
        VALUE(rt_first_set)        TYPE ty_ut_first_set .
    METHODS create_follow_set .
    METHODS create_follow_set2
      IMPORTING
        !is_symbol           TYPE zcl_ctxfreegram_grammar=>ty_us_symbol
        !it_preceding_symbol TYPE zcl_ctxfreegram_grammar=>ty_ut_symbol OPTIONAL
      RETURNING
        VALUE(rs_follow_set) TYPE ty_us_follow_set .
    METHODS create_goto_table .
    METHODS create_item_set_transit_tables .
    METHODS create_render_table
      IMPORTING
        !terminals    TYPE abap_bool DEFAULT abap_true
        !nonterminals TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result) TYPE REF TO data .
    METHODS do_the_closure
      CHANGING
        !cs_item_set TYPE zcl_ctxfreegram_grammar=>ty_us_item_set .
    METHODS error .
    METHODS format_rules
      RETURNING
        VALUE(rt_result) TYPE tt_rule .
    METHODS get_column_widths
      IMPORTING
        table         TYPE ANY TABLE
      RETURNING
        VALUE(result) TYPE zcl_ctxfreegram_grammar=>ty_column_widths.
    METHODS get_freetext
      IMPORTING
        !i_index        TYPE sytabix
      RETURNING
        VALUE(r_result) TYPE string .
    METHODS get_nonterminal
      IMPORTING
        !i_index        TYPE sytabix
      RETURNING
        VALUE(r_result) TYPE string .
    METHODS get_symbol
      IMPORTING
        !is_symbol      TYPE ty_us_symbol
      RETURNING
        VALUE(r_result) TYPE string .
    METHODS get_terminal
      IMPORTING
        !i_index        TYPE sytabix
        !i_abap         TYPE csequence DEFAULT abap_false
      RETURNING
        VALUE(r_result) TYPE string .
    METHODS remove_useless_reduce_actions .
ENDCLASS.



CLASS zcl_ctxfreegram_grammar IMPLEMENTATION.


  METHOD add_nonterminal.
    READ TABLE aut_symbol WITH KEY type  = lcs_symbol_type-nonterminal
                                   value = nonterminal->name
        ASSIGNING FIELD-SYMBOL(<ls_symbol2>).
*    READ TABLE aut_nonterminal WITH KEY nonterminal = nonterminal->name ASSIGNING FIELD-SYMBOL(<ls_nonterminal>).
    IF sy-subrc <> 0.

      APPEND INITIAL LINE TO aut_nonterminal ASSIGNING FIELD-SYMBOL(<ls_nonterminal>).
      <ls_nonterminal>-index        = sy-tabix.
      <ls_nonterminal>-nonterminal = nonterminal->name.
      <ls_nonterminal>-object       = nonterminal.

      APPEND INITIAL LINE TO aut_symbol ASSIGNING <ls_symbol2>.
      <ls_symbol2>-type   = lcs_symbol_type-nonterminal.
      <ls_symbol2>-index  = <ls_nonterminal>-index.
      <ls_symbol2>-value  = <ls_nonterminal>-nonterminal.
      <ls_symbol2>-object = nonterminal.

    ENDIF.

*    index = <ls_nonterminal>-index.
    index = <ls_symbol2>.
  ENDMETHOD.


  METHOD add_terminal.
    READ TABLE aut_symbol WITH KEY type  = lcs_symbol_type-terminal
                                   value = terminal->value
        ASSIGNING FIELD-SYMBOL(<ls_symbol2>).
*    READ TABLE aut_terminal WITH KEY terminal = terminal->value ASSIGNING FIELD-SYMBOL(<ls_terminal>).
    IF sy-subrc <> 0.

      APPEND INITIAL LINE TO aut_terminal ASSIGNING FIELD-SYMBOL(<ls_terminal>).
      <ls_terminal>-index    = sy-tabix.
      <ls_terminal>-terminal = terminal->value.
      <ls_terminal>-object   = terminal.

      APPEND INITIAL LINE TO aut_symbol ASSIGNING <ls_symbol2>.
      <ls_symbol2>-type   = lcs_symbol_type-terminal.
      <ls_symbol2>-index  = <ls_terminal>-index.
      <ls_symbol2>-value  = <ls_terminal>-terminal.
      <ls_symbol2>-object = terminal.

    ENDIF.

*    index = <ls_terminal>-index.
    index = <ls_symbol2>.
  ENDMETHOD.


  METHOD add_terminal_regex.
    DATA(regex) = terminal_regex->zif_ctxfreegram_rule_elem~get_text( ).
    READ TABLE aut_symbol WITH KEY type  = lcs_symbol_type-terminal_regex
                                   value = regex
        ASSIGNING FIELD-SYMBOL(<ls_symbol2>).
*    READ TABLE aut_terminal WITH KEY terminal = terminal->value ASSIGNING FIELD-SYMBOL(<ls_terminal>).
    IF sy-subrc <> 0.

      APPEND INITIAL LINE TO aut_terminal ASSIGNING FIELD-SYMBOL(<ls_terminal>).
      <ls_terminal>-index    = sy-tabix.
      <ls_terminal>-terminal = regex.
      <ls_terminal>-object   = terminal_regex.

      APPEND INITIAL LINE TO aut_symbol ASSIGNING <ls_symbol2>.
      <ls_symbol2>-type   = lcs_symbol_type-terminal_regex.
      <ls_symbol2>-index  = <ls_terminal>-index.
      <ls_symbol2>-value  = <ls_terminal>-terminal.
      <ls_symbol2>-object = terminal_regex.

    ENDIF.

*    index = <ls_terminal>-index.
    index = <ls_symbol2>.
  ENDMETHOD.


  METHOD check_lhs_rhs_is_bound.

    LOOP AT transmitted_rules INTO DATA(rule).

      IF rule-lhs IS NOT BOUND.
        RAISE EXCEPTION TYPE zcx_ctxfreegram
          EXPORTING
            error = zcx_ctxfreegram=>c_error-lhs_element_not_bound.
      ENDIF.

      IF rule-rhs IS BOUND.
        LOOP AT rule-rhs->get_elements( ) TRANSPORTING NO FIELDS
            WHERE table_line IS NOT BOUND.
          RAISE EXCEPTION TYPE zcx_ctxfreegram
            EXPORTING
              error = zcx_ctxfreegram=>c_error-rhs_element_not_bound.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_rhs_nonterminals_in_lhs.

    LOOP AT transmitted_rules REFERENCE INTO DATA(rule)
        WHERE rhs IS BOUND.
      DATA(rhs_elements) = rule->rhs->get_elements( ).
      LOOP AT rhs_elements INTO DATA(rhs_element).
        IF rhs_element IS INSTANCE OF zif_ctxfreegram_rule_nonterm.
          DATA(rhs_nonterminal) = CAST zif_ctxfreegram_rule_nonterm( rhs_element ).
          IF NOT line_exists( transmitted_rules[ lhs = rhs_nonterminal ] ).
            RAISE EXCEPTION TYPE zcx_ctxfreegram
              EXPORTING
                error = zcx_ctxfreegram=>c_error-rhs_nonterminal_wo_lhs.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_start_rule_appears_once.

    DATA(count) = 0.
    LOOP AT transmitted_rules REFERENCE INTO DATA(rule)
        WHERE lhs = au_start_rule.
      count = count + 1.
      IF count = 2.
        RAISE EXCEPTION TYPE zcx_ctxfreegram
          EXPORTING
            error = zcx_ctxfreegram=>c_error-start_rule_must_appear_once.
      ENDIF.
    ENDLOOP.

    IF count = 0.
      RAISE EXCEPTION TYPE zcx_ctxfreegram
        EXPORTING
          error = zcx_ctxfreegram=>c_error-missing_start_rule.
    ENDIF.

  ENDMETHOD.


  METHOD check_validity.

    IF au_start_rule IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_ctxfreegram
        EXPORTING
          error = zcx_ctxfreegram=>c_error-start_rule_not_bound.
    ENDIF.

    check_start_rule_appears_once( ).

    check_lhs_rhs_is_bound( ).

    check_rhs_nonterminals_in_lhs( ).

  ENDMETHOD.


  METHOD create.
    result = NEW zcl_ctxfreegram_grammar( ).
    result->au_start_rule = start_rule.
    result->transmitted_rules = rules.

    result->check_validity( ).

    result->create_2( ).
  ENDMETHOD.


  METHOD create_2.
    auo_terminal_end_of_input = zcl_ctxfreegram_factory=>create( )->new_terminal( terminal = '$' ).
    symbol_end_of_input = add_terminal( auo_terminal_end_of_input ).

    formatted_rules = format_rules( ).

    create_first_and_follow_sets( ).
    create_item_set_transit_tables( ).
    create_action_table( ).
    create_goto_table( ).
  ENDMETHOD.


  METHOD create_action_table.
    FIELD-SYMBOLS <ls_transition> TYPE ty_us_transition.
    FIELD-SYMBOLS <ls_action>     TYPE ty_us_action.
    FIELD-SYMBOLS <ls_item_set>   TYPE ty_us_item_set.
    FIELD-SYMBOLS <ls_item>       TYPE ty_us_item.
    FIELD-SYMBOLS <ls_rule>       TYPE zcl_ctxfreegram_grammar=>ty_us_rule.
    FIELD-SYMBOLS <ls_follow_set> TYPE ty_us_follow_set.
    FIELD-SYMBOLS <ls_symbol>     TYPE zcl_ctxfreegram_grammar=>ty_us_symbol.

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
         WHERE s_symbol-type = zcl_ctxfreegram_grammar=>lcs_symbol_type-terminal
            OR s_symbol-type = zcl_ctxfreegram_grammar=>lcs_symbol_type-terminal_regex.
      INSERT VALUE #( state          = <ls_transition>-item_set_index
                      terminal_index = <ls_transition>-s_symbol-index
                      action         = lcs_action-shift
                      index          = <ls_transition>-goto_item_set_index )
          INTO TABLE aut_action.
    ENDLOOP.

    " Part 2 of action determination  = list of REDUCE
    "   - there's none if the dot is at the end of RHS
    "   - If dot is not at the end of RHS:
    "        - if symbol at dot is a terminal, it's this terminal
    "        - if it's a nonterminal, take all symbols of the First Set of this nonterminal except "$"
    LOOP AT aut_item_set ASSIGNING <ls_item_set>.
      LOOP AT <ls_item_set>-items ASSIGNING <ls_item>
           WHERE     type              = lcs_item_type-kernel
                 AND s_dot_symbol-type = zcl_ctxfreegram_grammar=>lcs_symbol_type-nonterminal.
        LOOP AT formatted_rules ASSIGNING <ls_rule>
             WHERE     lhs_nonterm_index  = <ls_item>-s_dot_symbol-index
                   AND t_symbol          IS INITIAL.
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0 AND 0 = lines( <ls_rule>-t_symbol ).
          LOOP AT aut_follow_set ASSIGNING <ls_follow_set> WHERE symbol = <ls_item>-s_dot_symbol.
            LOOP AT <ls_follow_set>-t_follow_symbol ASSIGNING <ls_symbol>.
              INSERT VALUE #( state          = <ls_item_set>-index
                              terminal_index = <ls_symbol>-index
                              action         = lcs_action-reduce
                              index          = <ls_rule>-index )
                  INTO TABLE aut_action.
            ENDLOOP.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

      LOOP AT <ls_item_set>-items ASSIGNING <ls_item>
           WHERE     type           = lcs_item_type-kernel
                 AND dot_at_the_end = abap_true.
        READ TABLE aut_follow_set ASSIGNING <ls_follow_set>
             WITH KEY symbol-type  = zcl_ctxfreegram_grammar=>lcs_symbol_type-nonterminal
                      symbol-index = <ls_item>-lhs_nonterm_index.
        ASSERT sy-subrc = 0.
        LOOP AT <ls_follow_set>-t_follow_symbol ASSIGNING <ls_symbol>.
          INSERT VALUE #( state          = <ls_item_set>-index
                          terminal_index = <ls_symbol>-index
                          action         = COND #( WHEN <ls_item>-rule_index = 1
                                                   THEN lcs_action-accept
                                                   ELSE lcs_action-reduce )
                          index          = COND #( WHEN <ls_item>-rule_index = 1
                                                   THEN 0
                                                   ELSE <ls_item>-rule_index ) )
              INTO TABLE aut_action.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    " if both SHIFT and reduce are possible, then keep SHIFT
    LOOP AT aut_action ASSIGNING <ls_action> WHERE action = lcs_action-reduce.
      READ TABLE aut_action WITH KEY state          = <ls_action>-state
                                     terminal_index = <ls_action>-terminal_index
                                     action         = lcs_action-shift
           TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE aut_action.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_first_and_follow_sets.
    DATA ls_request    TYPE ty_is_request.
    DATA lt_symbol     TYPE zcl_ctxfreegram_grammar=>ty_ut_symbol.
    DATA ls_first_set  TYPE ty_us_first_set.
    DATA ls_follow_set TYPE ty_us_follow_set.

    REFRESH aut_first_set.
    REFRESH aut_follow_set.

    LOOP AT aut_symbol ASSIGNING FIELD-SYMBOL(<ls_symbol2>)
         WHERE    type = zcl_ctxfreegram_grammar=>lcs_symbol_type-terminal
               OR type = zcl_ctxfreegram_grammar=>lcs_symbol_type-nonterminal.

      CLEAR ls_request.
      ls_request-symbol-type  = <ls_symbol2>-type.
      ls_request-symbol-index = <ls_symbol2>-index.

      IF <ls_symbol2>-type = zcl_ctxfreegram_grammar=>lcs_symbol_type-nonterminal.
        ls_request-type = c_is_set_type-first_set.
        lt_symbol = create_first_or_follow_set( ls_request ).
        LOOP AT lt_symbol ASSIGNING FIELD-SYMBOL(<ls_symbol>).
          CLEAR ls_first_set.
          ls_first_set-lhs_nonterm_index = <ls_symbol2>-index.
          ls_first_set-s_rhs             = <ls_symbol>.
          APPEND ls_first_set TO aut_first_set.
        ENDLOOP.
      ENDIF.

      ls_request-type = c_is_set_type-follow_set.
      lt_symbol = create_first_or_follow_set( ls_request ).
      CLEAR ls_follow_set.
      ls_follow_set-symbol = <ls_symbol2>.
      LOOP AT lt_symbol ASSIGNING <ls_symbol>.
        COLLECT <ls_symbol> INTO ls_follow_set-t_follow_symbol.
      ENDLOOP.
      APPEND ls_follow_set TO aut_follow_set.

    ENDLOOP.
  ENDMETHOD.


  METHOD create_first_or_follow_set.
    DATA lt_processed_request      TYPE ty_it_request.
    DATA at_least_one_rule_epsilon TYPE abap_bool.
    DATA only_epsilons             TYPE abap_bool.
    DATA ls_request                TYPE ty_is_request.
    DATA lt_symbol                 TYPE zcl_ctxfreegram_grammar=>ty_ut_symbol.
    DATA l_tabix                   TYPE sytabix.

    FIELD-SYMBOLS <ls_rule>       TYPE zcl_ctxfreegram_grammar=>ty_us_rule.
    FIELD-SYMBOLS <ls_symbol>     TYPE zcl_ctxfreegram_grammar=>ty_us_symbol.
    FIELD-SYMBOLS <ls_symbol_bis> TYPE zcl_ctxfreegram_grammar=>ty_us_symbol.
    FIELD-SYMBOLS <ls_symbol2>    TYPE zcl_ctxfreegram_grammar=>ty_us_symbol.

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

        at_least_one_rule_epsilon = abap_false.
        LOOP AT formatted_rules ASSIGNING <ls_rule>
             WHERE lhs_nonterm_index = is_request-symbol-index.
          only_epsilons = abap_true.
          LOOP AT <ls_rule>-t_symbol ASSIGNING <ls_symbol>.
            IF <ls_symbol>-type = zcl_ctxfreegram_grammar=>lcs_symbol_type-nonterminal.
              CLEAR ls_request.
              ls_request-type   = c_is_set_type-first_set.
              ls_request-symbol = <ls_symbol>.
              lt_symbol = create_first_or_follow_set( is_request           = ls_request
                                                      it_processed_request = lt_processed_request ).
              LOOP AT lt_symbol ASSIGNING <ls_symbol_bis>.
                COLLECT <ls_symbol_bis> INTO rt_symbol.
              ENDLOOP.
            ELSEIF <ls_symbol> = symbol_end_of_input.
*            ELSEIF     <ls_symbol>-type  = zcl_ctxfreegram=>lcs_symbol_type-terminal
*                   AND <ls_symbol>-index = symbol_end_of_input-index.
              " Process next symbol in the RHS, its First Set will make the First Set of I_nonterminal
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
          " Add to the First set of requested nonterminal the follow set of nonterminal
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

        IF     is_request-symbol-type  = zcl_ctxfreegram_grammar=>lcs_symbol_type-nonterminal
           AND is_request-symbol-index = au_starting_nonterminal.
*          CLEAR ls_symbol.
*          ls_symbol-type   = zcl_ctxfreegram=>lcs_symbol_type-terminal.
*          ls_symbol-index  = au_terminal_end_of_input.
*          ls_symbol-object = auo_terminal_end_of_input.
*          COLLECT ls_symbol INTO rt_symbol.
          COLLECT symbol_end_of_input INTO rt_symbol.
        ELSE.

          LOOP AT formatted_rules ASSIGNING <ls_rule>.
            " for all terminals and nonterminals of RHS, the FOLLOW SET must be determined
            " except for symbol "end of input".
            LOOP AT <ls_rule>-t_symbol ASSIGNING <ls_symbol>
                 WHERE     type  = is_request-symbol-type
                       AND index = is_request-symbol-index.
              IF sy-tabix = lines( <ls_rule>-t_symbol ).
                " if symbol is at the end of RHS, then take the follow set of LHS of the production rule
                CLEAR ls_request.
                ls_request-type         = c_is_set_type-follow_set.
                ls_request-symbol-type  = zcl_ctxfreegram_grammar=>lcs_symbol_type-nonterminal.
                ls_request-symbol-index = <ls_rule>-lhs_nonterm_index.
                lt_symbol = create_first_or_follow_set( is_request           = ls_request
                                                        it_processed_request = lt_processed_request ).
                LOOP AT lt_symbol ASSIGNING <ls_symbol_bis>.
                  COLLECT <ls_symbol_bis> INTO rt_symbol.
                ENDLOOP.
              ELSE.
                " if symbol is NOT at the end of RHS, then based on the type of the next symbol:
                "   - if it's a terminal, add it to the follow set
                "   - if it's a nonterminal, add its whole first set to the follow set
                l_tabix = sy-tabix + 1.
                READ TABLE <ls_rule>-t_symbol INDEX l_tabix ASSIGNING <ls_symbol2>.
                ASSERT sy-subrc = 0.
                IF <ls_symbol2>-type = zcl_ctxfreegram_grammar=>lcs_symbol_type-terminal.
                  COLLECT <ls_symbol2> INTO rt_symbol.
                ELSE.
                  CLEAR ls_request.
                  ls_request-type   = c_is_set_type-first_set.
                  ls_request-symbol = <ls_symbol2>.
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
      <ls_nonterminal> TYPE zcl_ctxfreegram_grammar=>ty_us_nonterminal.

    REFRESH aut_first_set.
    LOOP AT aut_nonterminal ASSIGNING <ls_nonterminal>.
      lt_first_set = create_first_set2( <ls_nonterminal>-index ).
      APPEND LINES OF lt_first_set TO aut_first_set.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_first_set2.
    "! used to avoid recursive
    DATA lt_preceding_nonterminals TYPE ty_it_nonterminal.
    DATA at_least_one_rule_epsilon  TYPE abap_bool.
    DATA only_epsilons              TYPE abap_bool.
    DATA lt_first_set               TYPE ty_ut_first_set.
    DATA ls_first_set               TYPE ty_us_first_set.

    FIELD-SYMBOLS <ls_rule>         TYPE zcl_ctxfreegram_grammar=>ty_us_rule.
    FIELD-SYMBOLS <ls_symbol>       TYPE zcl_ctxfreegram_grammar=>ty_us_symbol.

    lt_preceding_nonterminals = it_preceding_nonterminals.
    APPEND i_nonterminal TO lt_preceding_nonterminals.

    at_least_one_rule_epsilon = abap_true.
    LOOP AT formatted_rules ASSIGNING <ls_rule>
         WHERE lhs_nonterm_index = i_nonterminal.
      only_epsilons = abap_true.
      LOOP AT <ls_rule>-t_symbol ASSIGNING <ls_symbol>.
        IF <ls_symbol>-type = zcl_ctxfreegram_grammar=>lcs_symbol_type-nonterminal.
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
        ELSEIF <ls_symbol> = symbol_end_of_input.
*        ELSEIF     <ls_symbol>-type  = zcl_ctxfreegram=>lcs_symbol_type-terminal
*               AND <ls_symbol>-index = au_terminal_end_of_input.
          " Process next symbol in the RHS, its First Set will make the First Set of I_nonterminal
          CONTINUE.
        ELSE. "terminal
          CLEAR ls_first_set.
          ls_first_set-lhs_nonterm_index = <ls_rule>-lhs_nonterm_index.
          ls_first_set-s_rhs             = <ls_symbol>.
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
      " Add to the First set of I_nonterminal the follow set of nonterminal
      ASSERT 1 = 1.
    ENDIF.
  ENDMETHOD.


  METHOD create_follow_set.
    DATA ls_symbol     TYPE zcl_ctxfreegram_grammar=>ty_us_symbol.
    DATA ls_follow_set TYPE ty_us_follow_set.

    FIELD-SYMBOLS:
      <ls_symbol2> TYPE zcl_ctxfreegram_grammar=>ty_us_symbol.

    REFRESH aut_follow_set.
    LOOP AT aut_symbol ASSIGNING <ls_symbol2>
         WHERE    type = zcl_ctxfreegram_grammar=>lcs_symbol_type-terminal
               OR type = zcl_ctxfreegram_grammar=>lcs_symbol_type-nonterminal.
      ls_symbol-type  = <ls_symbol2>-type.
      ls_symbol-index = <ls_symbol2>-index.
      ls_follow_set = create_follow_set2( is_symbol = ls_symbol ).
      APPEND ls_follow_set TO aut_follow_set.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_follow_set2.
    DATA ls_symbol           TYPE zcl_ctxfreegram_grammar=>ty_us_symbol.
    DATA lt_preceding_symbol TYPE zcl_ctxfreegram_grammar=>ty_ut_symbol.
    DATA ls_follow_set       TYPE ty_us_follow_set.
    DATA l_tabix             TYPE sytabix.

    FIELD-SYMBOLS <ls_rule>      TYPE zcl_ctxfreegram_grammar=>ty_us_rule.
    FIELD-SYMBOLS <ls_symbol>    TYPE zcl_ctxfreegram_grammar=>ty_us_symbol.
    FIELD-SYMBOLS <ls_symbol2>   TYPE zcl_ctxfreegram_grammar=>ty_us_symbol.
    FIELD-SYMBOLS <ls_first_set> TYPE ty_us_first_set.

    " find the symbol in RHS of all the rules
    rs_follow_set-symbol = is_symbol.
    rs_follow_set-symbol-index = is_symbol-index.

    IF     is_symbol-type  = zcl_ctxfreegram_grammar=>lcs_symbol_type-nonterminal
       AND is_symbol-index = au_starting_nonterminal.
*      ls_symbol-type  = zcl_ctxfreegram=>lcs_symbol_type-terminal.
*      ls_symbol-index = au_terminal_end_of_input.
*      COLLECT ls_symbol INTO rs_follow_set-t_follow_symbol.
      COLLECT symbol_end_of_input INTO rs_follow_set-t_follow_symbol.
      RETURN.
    ENDIF.

    " anti-recursive
    lt_preceding_symbol = it_preceding_symbol.
    INSERT is_symbol INTO TABLE lt_preceding_symbol.

    LOOP AT formatted_rules ASSIGNING <ls_rule>.
      " for all terminals and nonterminals of RHS, except for symbol "end of input", the FOLLOW SET must be determined
      LOOP AT <ls_rule>-t_symbol ASSIGNING <ls_symbol>
           WHERE     type  = is_symbol-type
                 AND index = is_symbol-index.
        IF sy-tabix = lines( <ls_rule>-t_symbol ).
          " if symbol is at the end of RHS, then take the follow set of LHS of the production rule
          ls_symbol-type  = zcl_ctxfreegram_grammar=>lcs_symbol_type-nonterminal.
          ls_symbol-index = <ls_rule>-lhs_nonterm_index.
          " anti-recursive
          READ TABLE lt_preceding_symbol WITH KEY type  = ls_symbol-type
                                                  index = ls_symbol-index
               TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            ls_follow_set = create_follow_set2( is_symbol           = ls_symbol
                                                it_preceding_symbol = lt_preceding_symbol ).
            LOOP AT ls_follow_set-t_follow_symbol ASSIGNING <ls_symbol2>.
              COLLECT <ls_symbol2> INTO rs_follow_set-t_follow_symbol.
            ENDLOOP.
          ENDIF.
        ELSE.
          " if symbol is NOT at the end of RHS, then take the first set of the next symbol
          l_tabix = sy-tabix + 1.
          READ TABLE <ls_rule>-t_symbol INDEX l_tabix ASSIGNING <ls_symbol2>.
          ASSERT sy-subrc = 0.
          IF <ls_symbol2>-type = zcl_ctxfreegram_grammar=>lcs_symbol_type-terminal.
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


  METHOD create_from_string_table.
    TYPES:
      BEGIN OF ts_terminal_constant,
        text   TYPE string,
        object TYPE REF TO zif_ctxfreegram_rule_term,
      END OF ts_terminal_constant.
    TYPES tt_terminal_constant TYPE SORTED TABLE OF ts_terminal_constant WITH UNIQUE KEY text.
    TYPES:
      BEGIN OF ts_terminal_regex,
        regex  TYPE string,
        object TYPE REF TO zif_ctxfreegram_rule_termregex,
      END OF ts_terminal_regex.
    TYPES tt_terminal_regex TYPE SORTED TABLE OF ts_terminal_regex WITH UNIQUE KEY regex.
    TYPES:
      BEGIN OF ts_nonterminal,
        text   TYPE string,
        object TYPE REF TO zif_ctxfreegram_rule_nonterm,
      END OF ts_nonterminal.
    TYPES tt_nonterminal TYPE SORTED TABLE OF ts_nonterminal WITH UNIQUE KEY text.

    DATA(factory) = zcl_ctxfreegram_factory=>create( ).

    DATA(terminal_constants) = VALUE tt_terminal_constant( ).
    DATA(terminal_regex_table) = VALUE tt_terminal_regex( ).
    DATA(nonterminals) = VALUE tt_nonterminal( ).
    DATA(rules) = VALUE zcl_ctxfreegram_grammar=>ty_rules( ).

    LOOP AT string_table REFERENCE INTO DATA(string).

      DATA(rule) = VALUE zcl_ctxfreegram_grammar=>ty_rule( ).
      DATA(rule_rhs_elements) = VALUE zcl_ctxfreegram_factory=>ty_elements( ).

      " \w : Placeholder for any Alphanumeric character including _
      FIND ALL OCCURRENCES OF REGEX `'[^']+'|\[[^\]]+|\w+` IN string->* RESULTS DATA(matches).

      LOOP AT matches REFERENCE INTO DATA(match).
        DATA(match_text) = string->*+match->offset(match->length).
        IF match_text(1) <> ` `.

          CASE match_text(1).

            WHEN `'`.
              DATA(terminal_constant_value) = substring( val = match_text
                                                         off = 1
                                                         len = strlen( match_text ) - 2 ).
              DATA(terminal_constant) = REF #( terminal_constants[ text = terminal_constant_value ] OPTIONAL ).
              IF terminal_constant IS NOT BOUND.
                INSERT VALUE #( text   = terminal_constant_value
                                object = factory->new_terminal( terminal_constant_value ) )
                  INTO TABLE terminal_constants
                  REFERENCE INTO terminal_constant.
              ENDIF.
              INSERT terminal_constant->object INTO TABLE rule_rhs_elements.

            WHEN `[`.
              DATA(terminal_regex) = substring( val = match_text
                                                off = 1
                                                len = strlen( match_text ) - 2 ).
              DATA(terminal_regex_item) = REF #( terminal_regex_table[ regex = terminal_regex ] OPTIONAL ).
              IF terminal_regex_item IS NOT BOUND.
                INSERT VALUE #( regex  = terminal_regex
                                object = factory->new_terminal_regex( terminal_regex ) )
                  INTO TABLE terminal_regex_table
                  REFERENCE INTO terminal_regex_item.
              ENDIF.
              INSERT terminal_regex_item->object INTO TABLE rule_rhs_elements.

            WHEN OTHERS.
              DATA(nonterminal) = REF #( nonterminals[ text = match_text ] OPTIONAL ).
              IF nonterminal IS NOT BOUND.
                INSERT VALUE #( text       = match_text
                                object     = factory->new_nonterminal( match_text ) )
                  INTO TABLE nonterminals
                  REFERENCE INTO nonterminal.
              ENDIF.
              IF rule-lhs IS NOT BOUND.
                rule-lhs = nonterminal->object.
              ELSE.
                INSERT nonterminal->object INTO TABLE rule_rhs_elements.
              ENDIF.
          ENDCASE.
        ENDIF.
      ENDLOOP.

      IF rule_rhs_elements IS NOT INITIAL.
        CASE lines( rule_rhs_elements ).
          WHEN 0.
            rule-rhs = VALUE #( ).
          WHEN 1.
            rule-rhs = rule_rhs_elements[ 1 ].
          WHEN OTHERS.
            rule-rhs = factory->new_sequence( rule_rhs_elements ).
        ENDCASE.
      ENDIF.

      INSERT rule INTO TABLE rules.

    ENDLOOP.

    DATA(start_rule_object) = VALUE #( nonterminals[ text = CONV string( start_rule ) ]-object OPTIONAL ).
    IF start_rule_object IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING error = zcx_ctxfreegram=>c_error-missing_start_rule.
    ENDIF.

    result = zcl_ctxfreegram_grammar=>create( start_rule = start_rule_object
                                              rules      = rules ).
  ENDMETHOD.


  METHOD create_goto_table.
    aut_goto = VALUE #( FOR <ls_transition> IN aut_transition
                        WHERE ( s_symbol-type = lcs_symbol_type-nonterminal )
                        ( state             = <ls_transition>-item_set_index
                          lhs_nonterm_index = <ls_transition>-s_symbol-index
                          next_state        = <ls_transition>-goto_item_set_index ) ).
  ENDMETHOD.


  METHOD create_item_set_transit_tables.
    DATA lt_symbol       TYPE zcl_ctxfreegram_grammar=>ty_ut_symbol.
    DATA l_goto_item_set TYPE sytabix.
    DATA ls_item_new     TYPE ty_us_item.
    DATA l_already_found TYPE flag.

    FIELD-SYMBOLS <item_set>     TYPE ty_us_item_set.
    FIELD-SYMBOLS <ls_item>         TYPE ty_us_item.
    FIELD-SYMBOLS <ls_rule>         TYPE zcl_ctxfreegram_grammar=>ty_us_rule.
    FIELD-SYMBOLS <ls_symbol_new>   TYPE zcl_ctxfreegram_grammar=>ty_us_symbol.
    FIELD-SYMBOLS <ls_item_set_bis> TYPE ty_us_item_set.
    FIELD-SYMBOLS <ls_item_bis>     TYPE ty_us_item.
    FIELD-SYMBOLS <ls_transition>   TYPE ty_us_transition.
    FIELD-SYMBOLS <ls_symbol> TYPE zcl_ctxfreegram_grammar=>ty_us_symbol.

    REFRESH aut_transition.

    " Create item_set with only item_set(1)
    aut_item_set = VALUE #(
        LET <aux_rule> = formatted_rules[ au_starting_nonterminal ] IN
        ( index             = 1
          start_symbol      = aut_symbol[ type  = lcs_symbol_type-nonterminal
                                          value = au_start_rule->name ]
          items             = VALUE #( ( rule_index        = <aux_rule>-index
                                          type              = lcs_item_type-kernel
                                          dot_index         = 1
                                          s_dot_symbol      = VALUE #( <aux_rule>-t_symbol[ 1 ] OPTIONAL )
                                          lhs_nonterm_index = <aux_rule>-lhs_nonterm_index
                                          nr_of_rhs_symbols = lines( <aux_rule>-t_symbol )
                                          dot_at_the_end    = xsdbool( <aux_rule>-t_symbol IS INITIAL ) ) ) ) ).

    " Process all item sets.
    " At the beginning, there's only one item set
    " But this loop adds new item sets.
    LOOP AT aut_item_set ASSIGNING <item_set>.

      "--------------------------------
      " Do the "closure"
      " = add the items to the current item set
      "--------------------------------
      do_the_closure( CHANGING cs_item_set = <item_set> ).

      "--------------------------------
      " Calculate the new item sets
      "--------------------------------

*      " list of symbols of current item set, positioned on DOT_INDEX
*      TYPES:
*        BEGIN OF ts_item_set,
*          rule_index   TYPE i,
*          dot_index    TYPE i,
*          "! Hex code to be equivalent to descending order T then N, to have Terminals first, Nonterminals second.
*          type_descending TYPE xstring,
*        END OF ts_item_set.
*      TYPES tt_item_set TYPE SORTED TABLE OF ts_item_set WITH UNIQUE KEY rule_index dot_index
*                          WITH NON-UNIQUE SORTED KEY by_type_descending COMPONENTS type_descending.

*      DATA(lt_symbol) = VALUE tt_item_set( ).

      REFRESH lt_symbol.
      LOOP AT <item_set>-items ASSIGNING <ls_item>.
        IF <ls_item>-s_dot_symbol IS NOT INITIAL.
*          CONVERT TEXT <ls_item>-s_dot_symbol-type INTO SORTABLE CODE DATA(sortable_code).
*          DATA(sortable_code_descending) = BIT-NOT sortable_code.
*          INSERT VALUE #( rule_index      = <ls_item>-rule_index
*                          dot_index       = <ls_item>-dot_index
*                          type_descending = sortable_code_descending )
*                INTO TABLE lt_symbol.
          COLLECT <ls_item>-s_dot_symbol INTO lt_symbol.
*        ELSE.
*          DATA(follow_set) = REF #( aut_follow_set[ symbol-type  = 'N'
*                                                    symbol-index = <ls_item>-lhs_nonterm_index ]
*                                                  OPTIONAL ).
*          IF follow_set IS BOUND.
*            " This logic is intended to process optional rules.
*            " The current logic is probably wrong, it should not be the follow set
*            " of the nonterminal but the following symbol(s) in the current rule
*            " e.g. in this grammar
*            "      A: B "C" B "D"
*            "      B: "E"
*            "      B:
*            " the first B of A is followed by "C" only, the second B of A is followed by "D" only.
*            LOOP AT follow_set->t_follow_symbol REFERENCE INTO DATA(follow_symbol).
*              COLLECT follow_symbol->* INTO lt_symbol.
*            ENDLOOP.
*          ENDIF.
        ENDIF.
      ENDLOOP.

      " sort to place the terminals before the nonterminals
      SORT lt_symbol BY type DESCENDING index ASCENDING.

      " For each distinct next symbol of the current item set, create a new item set.
      " (symbol can be terminal or nonterminal)
      LOOP AT lt_symbol ASSIGNING <ls_symbol>.
*            USING KEY by_type_descending.

        CLEAR l_goto_item_set.

        DATA(ls_item_set_new) = VALUE ty_us_item_set(
            index             = lines( aut_item_set ) + 1
            start_symbol      = aut_symbol[ type  = <ls_symbol>-type
                                            index = <ls_symbol>-index ] ).
*            rule_index   = <ls_rule>-index ).
*            rule_lhs     = <ls_rule>-lhs_nonterm_index ).

        " For each item which has this symbol as next symbol, append the item to the new item set as kernel item.
        LOOP AT <item_set>-items ASSIGNING <ls_item>
            WHERE s_dot_symbol = ls_item_set_new-start_symbol.

          READ TABLE formatted_rules INDEX <ls_item>-rule_index ASSIGNING <ls_rule>.
          IF sy-subrc <> 0.
            error( ).
          ENDIF.

          " create an item set with these items
          CLEAR ls_item_new.
          ls_item_new-type       = lcs_item_type-kernel.
          ls_item_new-rule_index = <ls_item>-rule_index.
          " determine next dot index
          ls_item_new-dot_index  = <ls_item>-dot_index + 1.
          READ TABLE formatted_rules INDEX <ls_item>-rule_index ASSIGNING <ls_rule>.
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
          l_already_found = abap_false.
          LOOP AT aut_item_set ASSIGNING <ls_item_set_bis>.
            LOOP AT <ls_item_set_bis>-items ASSIGNING <ls_item_bis>
                 WHERE     dot_index  = ls_item_new-dot_index
                       AND rule_index = ls_item_new-rule_index.
              l_already_found = abap_true.
              EXIT.
            ENDLOOP.
            IF l_already_found = abap_true.
              EXIT.
            ENDIF.
          ENDLOOP.

          IF l_already_found = abap_true.
            l_goto_item_set = <ls_item_set_bis>-index.
          ELSE.
            APPEND ls_item_new TO ls_item_set_new-items.

            l_goto_item_set = ls_item_set_new-index.

            IF ls_item_new-dot_at_the_end = abap_true.
              ls_item_set_new-index_of_reduced_rule = ls_item_new-rule_index.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF ls_item_set_new-items IS NOT INITIAL.
          INSERT ls_item_set_new INTO TABLE aut_item_set.
        ENDIF.

        " add the transition of the starting item to the target item set.
        APPEND INITIAL LINE TO aut_transition ASSIGNING <ls_transition>.
        <ls_transition>-item_set_index      = <item_set>-index.
        <ls_transition>-s_symbol            = <ls_symbol>.
        <ls_transition>-goto_item_set_index = l_goto_item_set.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_render_table.
    DATA(components) = VALUE abap_component_tab( LET type = cl_abap_elemdescr=>get_string( ) IN
                                                 ( name = 'item_set' type = cl_abap_elemdescr=>get_string( ) )
                                                 ( LINES OF COND #( WHEN terminals = abap_true
                                                                    THEN VALUE #( FOR <terminal> IN aut_terminal
                                                                                  ( name = 'T' && <terminal>-index
                                                                                    type = type ) ) ) )
                                                 ( LINES OF COND #( WHEN nonterminals = abap_true
                                                                    THEN VALUE #( FOR <nonterminal> IN aut_nonterminal
                                                                                  ( name = 'N' && <nonterminal>-index
                                                                                    type = type ) ) ) ) ).

    DATA(rtti_table) = cl_abap_tabledescr=>get( p_line_type = cl_abap_structdescr=>get( components ) ).
    CREATE DATA result TYPE HANDLE rtti_table.
  ENDMETHOD.


  METHOD do_the_closure.
    "! list of all closed nonterminals
    TYPES tt_processed_nonterminals TYPE SORTED TABLE OF sytabix WITH UNIQUE KEY table_line.
    FIELD-SYMBOLS <ls_item_new> TYPE zcl_ctxfreegram_grammar=>ty_us_item.

    DATA(lt_processed_nonterminals) = VALUE tt_processed_nonterminals( ).
    LOOP AT cs_item_set-items REFERENCE INTO DATA(item)
          WHERE s_dot_symbol-type = zcl_ctxfreegram_grammar=>lcs_symbol_type-nonterminal.

      " Process each nonterminal only once to avoir endless loop
      IF NOT line_exists( lt_processed_nonterminals[ table_line = item->s_dot_symbol-index ] ).
        INSERT item->s_dot_symbol-index INTO TABLE lt_processed_nonterminals.

        LOOP AT formatted_rules REFERENCE INTO DATA(rule)
             WHERE lhs_nonterm_index = item->s_dot_symbol-index.
          APPEND INITIAL LINE TO cs_item_set-items ASSIGNING <ls_item_new>.
          <ls_item_new>-rule_index = rule->index.
          <ls_item_new>-type       = lcs_item_type-closure.
          READ TABLE rule->t_symbol REFERENCE INTO DATA(symbol) INDEX 1.
          IF sy-subrc <> 0.
            " in case the RHS is empty (optional nonterminal)
            <ls_item_new>-dot_at_the_end = abap_true.
          ELSE.
            <ls_item_new>-s_dot_symbol   = symbol->*.
            <ls_item_new>-dot_at_the_end = abap_false.
          ENDIF.
          <ls_item_new>-dot_index = 1.
          <ls_item_new>-lhs_nonterm_index = rule->lhs_nonterm_index.
          <ls_item_new>-nr_of_rhs_symbols = lines( rule->t_symbol ).
          IF <ls_item_new>-dot_at_the_end = abap_true.
            cs_item_set-index_of_reduced_rule = <ls_item_new>-rule_index.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD error.
    BREAK-POINT.
  ENDMETHOD.


  METHOD format_rules.
    TYPES ty_elements TYPE STANDARD TABLE OF REF TO zif_ctxfreegram_rule_elem WITH EMPTY KEY.

    DATA ty_elements TYPE ty_elements.

    LOOP AT transmitted_rules INTO DATA(rule).

      DATA(symbol) = add_nonterminal( rule-lhs ).

      APPEND INITIAL LINE TO rt_result ASSIGNING FIELD-SYMBOL(<ls_rule>).

      <ls_rule>-lhs               = rule-lhs.
      <ls_rule>-rhs               = rule-rhs.
      <ls_rule>-index             = sy-tabix.
      <ls_rule>-lhs_nonterm_index = symbol-index.
      <ls_rule>-plain_text        = |{ rule-lhs->get_text( ) }: { COND #( WHEN rule-rhs IS BOUND THEN rule-rhs->get_text( ) ) }|.

      IF rule-lhs = au_start_rule.
        au_starting_nonterminal = <ls_rule>-lhs_nonterm_index.
      ENDIF.

      IF rule-rhs IS BOUND.
        DATA(elements) = VALUE ty_elements( ( rule-rhs ) ).
        LOOP AT elements INTO DATA(element).

          DATA(new_elements) = VALUE ty_elements( ).

          CASE element->type.
            WHEN element->c_type-nonterminal.
              DATA(nonterminal) = CAST zif_ctxfreegram_rule_nonterm( element ).
              symbol = add_nonterminal( nonterminal ).
              INSERT symbol INTO TABLE <ls_rule>-t_symbol.
            WHEN element->c_type-terminal.
              DATA(terminal) = CAST zif_ctxfreegram_rule_term( element ).
              symbol = add_terminal( terminal ).
              INSERT symbol INTO TABLE <ls_rule>-t_symbol.
            WHEN element->c_type-terminal_regex.
              DATA(terminal_regex) = CAST zif_ctxfreegram_rule_termregex( element ).
              symbol = add_terminal_regex( terminal_regex ).
              INSERT symbol INTO TABLE <ls_rule>-t_symbol.
            WHEN element->c_type-sequence.
              DATA(sequence) = CAST zif_ctxfreegram_rule_seq( element ).
              new_elements = VALUE #( FOR aux_element IN sequence->elements
                                      ( aux_element ) ).
            WHEN OTHERS.
              RAISE EXCEPTION TYPE zcx_ctxfreegram_unexpected
                EXPORTING
                  text  = 'Rule "&1" contains an element of invalid type "&2"'
                  msgv1 = EXACT #( rule-lhs->get_text( ) )
                  msgv2 = EXACT #( element->type ).
          ENDCASE.

          LOOP AT new_elements INTO element.
            IF line_exists( elements[ table_line = element ] ).
              DELETE new_elements USING KEY loop_key.
            ENDIF.
          ENDLOOP.
          APPEND LINES OF new_elements TO elements.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_column_widths.
    CONSTANTS no_of_blanks_between_columns TYPE i VALUE 2.

    DATA(components) = CAST cl_abap_structdescr( CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( table ) )->get_table_line_type( ) )->get_components( ).
    DO lines( components ) TIMES.
      DATA(max_width) = 0.
      LOOP AT table ASSIGNING FIELD-SYMBOL(<line>).
        ASSIGN COMPONENT sy-index OF STRUCTURE <line> TO FIELD-SYMBOL(<comp>).
        max_width = nmax( val1 = max_width
                          val2 = strlen( |{ <comp> }| ) ).
      ENDLOOP.
      max_width = max_width + no_of_blanks_between_columns.
      APPEND max_width TO result.
    ENDDO.

  ENDMETHOD.


  METHOD get_freetext.
    FIELD-SYMBOLS <ls_freetext> TYPE zcl_ctxfreegram_grammar=>ty_us_freetext.

    READ TABLE aut_freetext
         WITH KEY index = i_index
         ASSIGNING <ls_freetext>.
    ASSERT sy-subrc = 0.
    r_result = <ls_freetext>-value.
  ENDMETHOD.


  METHOD get_nonterminal.
    FIELD-SYMBOLS <ls_nonterminal> TYPE zcl_ctxfreegram_grammar=>ty_us_nonterminal.

    READ TABLE aut_nonterminal
         WITH KEY index = i_index
         ASSIGNING <ls_nonterminal>.
    ASSERT sy-subrc = 0.
    r_result = <ls_nonterminal>-nonterminal.
*  ENDMETHOD.
*
*  METHOD render.
*    DATA l_rule                       TYPE string.
*    DATA l_previous_symbol_is_t_or_nt TYPE abap_bool.
*    DATA l_string                     TYPE string.
*
*    FIELD-SYMBOLS <ls_rule>   TYPE ty_us_rule.
*    FIELD-SYMBOLS <ls_symbol> TYPE zcl_ctxfreegram_grammar=>ty_us_symbol.
*
*    REFRESH et_text.
*    CLEAR e_text.
*    REFRESH et_stream.
*    LOOP AT formatted_rules ASSIGNING <ls_rule>.
*      CLEAR l_rule.
*      l_rule = get_nonterminal( <ls_rule>-lhs_nonterm_index ).
*      CONCATENATE l_rule ` (*` <ls_rule>-name_alv `*) = ` INTO l_rule.
*      l_previous_symbol_is_t_or_nt = abap_false.
*      LOOP AT <ls_rule>-t_symbol_including_comments ASSIGNING <ls_symbol>.
*        l_string = get_symbol( <ls_symbol> ).
*        IF     l_previous_symbol_is_t_or_nt = abap_true
*           AND (    <ls_symbol>-type = lcs_symbol_type-terminal
*                 OR <ls_symbol>-type = lcs_symbol_type-nonterminal ).
*          CONCATENATE l_rule `, ` l_string INTO l_rule.
*          l_previous_symbol_is_t_or_nt = abap_false.
*        ELSE.
*          CONCATENATE l_rule ` ` l_string INTO l_rule.
*        ENDIF.
*        IF    <ls_symbol>-type = lcs_symbol_type-terminal
*           OR <ls_symbol>-type = lcs_symbol_type-nonterminal.
*          l_previous_symbol_is_t_or_nt = abap_true.
*        ENDIF.
*      ENDLOOP.
*      CONCATENATE l_rule ` ;` INTO l_rule.
**      IF et_text IS SUPPLIED.
**        APPEND l_rule TO et_text.
**      ENDIF.
*      IF e_text IS SUPPLIED.
**      OR et_stream IS SUPPLIED.
*        CONCATENATE e_text cl_abap_char_utilities=>cr_lf l_rule INTO e_text.
*      ENDIF.
*    ENDLOOP.
**    IF et_stream IS SUPPLIED.
**      CALL FUNCTION 'SWA_STRING_TO_TABLE'
**        EXPORTING
**          character_string = e_text
**        IMPORTING
**          character_table  = et_stream.
**    ENDIF.
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
    FIELD-SYMBOLS <ls_terminal> TYPE zcl_ctxfreegram_grammar=>ty_us_terminal.

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
    DATA do_delete TYPE abap_bool.
    DATA: ls_symbol TYPE zcl_ctxfreegram_grammar=>ty_us_symbol.

    FIELD-SYMBOLS <ls_action>     TYPE ty_us_action.
    FIELD-SYMBOLS <ls_item_set>   TYPE ty_us_item_set.
    FIELD-SYMBOLS <ls_item>       TYPE ty_us_item.
    FIELD-SYMBOLS <ls_follow_set> TYPE ty_us_follow_set.

    LOOP AT aut_action ASSIGNING <ls_action>
         WHERE action = lcs_action-reduce.
      READ TABLE aut_item_set WITH KEY index = <ls_action>-state ASSIGNING <ls_item_set>.
      ASSERT sy-subrc = 0.
*      IF <ls_item_set>-rule_lhs = 0.
*        " delete all REDUCE of the starting rule, except for symbol "end of input"
*        IF <ls_action>-terminal_index <> symbol_end_of_input-index.
*          DELETE aut_action.
*        ENDIF.
*      ELSE.
      " delete REDUCE if none of follow set of current symbols of item set doesn't contain the terminal of the action.
      do_delete = abap_true.
      LOOP AT <ls_item_set>-items ASSIGNING <ls_item>
           WHERE type = lcs_item_type-kernel.
        READ TABLE aut_follow_set WITH KEY symbol = <ls_item>-s_dot_symbol ASSIGNING <ls_follow_set>.
        ASSERT sy-subrc = 0.
        CLEAR ls_symbol.
        ls_symbol-type  = zcl_ctxfreegram_grammar=>lcs_symbol_type-terminal.
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
*      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD render_action_goto_table.
*    DATA ref_table TYPE REF TO data.

    FIELD-SYMBOLS <actions> TYPE STANDARD TABLE.

    DATA(ref_table) = create_render_table( ).
    ASSIGN ref_table->* TO <actions>.

    " HEADING
    APPEND INITIAL LINE TO <actions> ASSIGNING FIELD-SYMBOL(<action>).
    LOOP AT aut_symbol ASSIGNING FIELD-SYMBOL(<symbol>).
      DATA(compname) = |{ COND #( WHEN <symbol>-type = lcs_symbol_type-terminal
                                    OR <symbol>-type = lcs_symbol_type-terminal_regex
                                  THEN 'T'
                                  ELSE 'N' )
                        }{ <symbol>-index }|.
      ASSIGN COMPONENT compname OF STRUCTURE <action> TO FIELD-SYMBOL(<comp>).
      <comp> = <symbol>-object->get_text( ).
    ENDLOOP.

    LOOP AT aut_item_set REFERENCE INTO DATA(item_set).

      APPEND INITIAL LINE TO <actions> ASSIGNING <action>.

      ASSIGN COMPONENT 'ITEM_SET' OF STRUCTURE <action> TO <comp>.
      <comp> = |{ item_set->index }|.

      " ACTIONS
      LOOP AT aut_action REFERENCE INTO DATA(action)
           WHERE state = item_set->index.
        compname = |T{ action->terminal_index }|.
        ASSIGN COMPONENT compname OF STRUCTURE <action> TO <comp>.
        IF sy-subrc <> 0.
          error( ).
        ENDIF.
        <comp> = COND string( WHEN action->action = 'a' THEN |acc| ELSE |{ action->action }{ action->index }| ).
      ENDLOOP.

      " GOTO table
      LOOP AT aut_transition REFERENCE INTO DATA(transition)
           WHERE     s_symbol-type  = lcs_symbol_type-nonterminal
                 AND item_set_index = item_set->index.
        compname = |N{ transition->s_symbol-index }|.
        ASSIGN COMPONENT compname OF STRUCTURE <action> TO <comp>.
        IF sy-subrc <> 0.
          error( ).
        ENDIF.
        <comp> = transition->goto_item_set_index. "cond string( when action->action = 'a' then |acc| else |{ action->action }{ action->index }| ).
      ENDLOOP.

    ENDLOOP.

    result = zcl_ctxfreegram_utilities=>render_table( <actions> ).
  ENDMETHOD.


  METHOD render_first_sets.
    result = concat_lines_of(
        table = VALUE string_table(
            FOR GROUPS <group_by_lhs_nonterm_index> OF <first_set> IN aut_first_set
            GROUP BY <first_set>-lhs_nonterm_index
            ( |* First set of nonterminal [{ aut_nonterminal[
                                                  index = <group_by_lhs_nonterm_index> ]-nonterminal }] can be any of these nonterminals and terminals:\n| )
            ( LINES OF VALUE #( FOR <first_set_in_group> IN GROUP <group_by_lhs_nonterm_index>
                                ( |** { <first_set_in_group>-s_rhs-object->get_text( ) }\n| ) ) ) ) ).
  ENDMETHOD.


  METHOD render_follow_sets.
    result = concat_lines_of(
        table = VALUE string_table(
            FOR <follow_set> IN aut_follow_set INDEX INTO i
            ( |* Follow set of terminal or nonterminal [{ <follow_set>-symbol-object->get_text( ) }] can be any of these terminals:\n| )
            ( LINES OF VALUE #( FOR <follow_terminal> IN <follow_set>-t_follow_symbol
                                ( |** { <follow_terminal>-object->get_text( ) }\n| ) ) ) ) ).
  ENDMETHOD.


  METHOD render_item_sets.
    result = concat_lines_of(
        table = VALUE string_table(
            FOR <item_set> IN aut_item_set INDEX INTO i
            ( |* Item set { i }:\n| )
            ( LINES OF VALUE #(
                  FOR <item> IN <item_set>-items
                  LET rule = formatted_rules[ <item>-rule_index ] IN
                  (  |** Item { COND #( WHEN <item>-type = lcs_item_type-kernel THEN `kernel` ELSE `closure` ) }: { rule-lhs->get_text( ) } = |
                  && COND #( WHEN rule-rhs IS NOT BOUND
                          THEN `◆`
                          ELSE concat_lines_of(
                              sep   = ` `
                              table = COND string_table(
                                  LET elements = rule-rhs->get_elements( )
                                  IN
                                  WHEN elements IS INITIAL
                                    THEN VALUE #( ( `◆` ) )
                                  ELSE
                                    VALUE #( FOR elem IN elements INDEX INTO j
                                             ( COND #( WHEN j = <item>-dot_index THEN `◆ ` )
                                               && elem->get_text( )
                                               && COND #( WHEN     <item>-dot_at_the_end = abap_true
                                                               AND j                     = <item>-nr_of_rhs_symbols
                                                          THEN ` ◆` ) ) ) ) ) )
                  && |\n| ) ) ) ) ).
  ENDMETHOD.


  METHOD render_rules.
    result = REDUCE #(
        INIT t = ``
        FOR rule IN formatted_rules INDEX INTO rule_number
        NEXT t = t && |{ rule_number WIDTH = 4 ALIGN = LEFT } { rule-lhs->get_text( ) }: { rule-rhs->get_text( ) }\n| ).
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
    LOOP AT aut_item_set REFERENCE INTO DATA(item_set).

      APPEND INITIAL LINE TO <transitions> ASSIGNING <transition>.

      ASSIGN COMPONENT 'item_set' OF STRUCTURE <transition> TO <comp>.
      <comp> = |{ item_set->index }|.

      LOOP AT aut_transition REFERENCE INTO DATA(transition)
           WHERE item_set_index = item_set->index.
        compname = |{ COND #( WHEN transition->s_symbol-type = lcs_symbol_type-terminal THEN 'T' ELSE 'N' ) }{ transition->s_symbol-index }|.
        ASSIGN COMPONENT compname OF STRUCTURE <transition> TO <comp>.
        IF sy-subrc <> 0.
          error( ).
        ENDIF.
        <comp> = transition->goto_item_set_index.
      ENDLOOP.

    ENDLOOP.

    result = zcl_ctxfreegram_utilities=>render_table( <transitions> ).
  ENDMETHOD.

ENDCLASS.
