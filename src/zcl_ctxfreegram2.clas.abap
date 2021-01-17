CLASS zcl_ctxfreegram2 DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS factory
      RETURNING
        VALUE(instance) TYPE REF TO zcl_ctxfreegram2.

    CLASS-METHODS class_constructor.
    METHODS constructor.

  PRIVATE SECTION.

    TYPES ty_rules TYPE STANDARD TABLE OF REF TO lcl_rule WITH EMPTY KEY.
    TYPES ty_symbols TYPE STANDARD TABLE OF REF TO lcl_symbol WITH EMPTY KEY.

    METHODS load_rules
      IMPORTING
        rules TYPE ty_rules
      RAISING
        zcx_ctxfreegram.

    METHODS create_first_and_follow_sets.

    TYPES ty_ut_symbol TYPE STANDARD TABLE OF REF TO lcl_symbol WITH EMPTY KEY.

    METHODS create_first_set
      IMPORTING
        non_terminal   TYPE REF TO lcl_non_terminal
        process_once   TYPE REF TO lcl_ffs_process_once
      RETURNING
        VALUE(symbols) TYPE REF TO lcl_symbols.

    "! NB: works only if create_first_set is called first.
    METHODS create_follow_set
      IMPORTING
        symbol         TYPE REF TO lcl_symbol
        process_once   TYPE REF TO lcl_ffs_process_once
      RETURNING
        VALUE(symbols) TYPE REF TO lcl_symbols.

    METHODS create_item_set_transit_tables.

    METHODS render_item_sets
      RETURNING
        VALUE(result) TYPE string.

    METHODS render_transitions
      RETURNING
        VALUE(result) TYPE string.

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

    METHODS create_first_sets
      RAISING
        zcx_ctxfreegram.

    METHODS create_follow_sets
      RAISING
        zcx_ctxfreegram.

    METHODS render_first_sets
      RETURNING
        VALUE(result) TYPE string.

    METHODS render_follow_sets
      RETURNING
        VALUE(result) TYPE string.

    DATA: start_rule    TYPE REF TO lcl_rule,
          end_of_input  TYPE REF TO lcl_terminal,
          rules         TYPE ty_rules,
          item_sets     TYPE REF TO lcl_item_sets,
          transitions   TYPE REF TO lcl_transitions,
          first_sets    TYPE REF TO lcl_first_sets,
          follow_sets   TYPE REF TO lcl_follow_sets,
          non_terminals TYPE TABLE OF REF TO lcl_non_terminal,
          terminals     TYPE TABLE OF REF TO lcl_terminal,
          symbols       TYPE ty_symbols.

    TYPES: ty_u_symbol_type TYPE c LENGTH 1,
           BEGIN OF ty_us_symbol,
             type   TYPE ty_u_symbol_type,
             index  TYPE sytabix,
             value  TYPE string,
             object TYPE REF TO zif_ctxfreegram_rule_elem,
           END OF ty_us_symbol,
           BEGIN OF ty_us_first_set,
             non_terminal TYPE REF TO lcl_non_terminal,
             first_set    TYPE REF TO lcl_symbols,
           END OF ty_us_first_set,
           BEGIN OF ty_us_follow_set,
             symbol     TYPE REF TO lcl_symbol,
             follow_set TYPE REF TO lcl_symbols,
           END OF ty_us_follow_set,
           ty_ut_follow_set TYPE TABLE OF ty_us_follow_set WITH NON-UNIQUE KEY symbol, "lhs rhs_terminal_index rhs_follow_set,.
           ty_ut_first_set  TYPE STANDARD TABLE OF ty_us_first_set WITH EMPTY KEY. "NON-UNIQUE KEY lhs_nonterm_index s_rhs.
    DATA: aut_first_set  TYPE ty_ut_first_set, "REF TO lcl_first_sets,
          aut_follow_set TYPE ty_ut_follow_set. "REF TO lcl_follow_sets,

ENDCLASS.



CLASS zcl_ctxfreegram2 IMPLEMENTATION.


  METHOD constructor.
    end_of_input = NEW lcl_terminal( 'EOF' ).
  ENDMETHOD.


  METHOD load_rules.

    me->start_rule = rules[ 1 ].
    IF start_rule IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING error = zcx_ctxfreegram=>c_error-start_rule_not_bound.
    ENDIF.

    me->rules = rules.

    DATA(symbols_temp) = VALUE ty_symbols(
        FOR rule IN rules
        ( rule->lhs )
        ( LINES OF VALUE #( FOR elem IN rule->rhs ( elem ) ) ) ).
    symbols = VALUE #( FOR GROUPS group_symbols OF symbol IN symbols_temp GROUP BY ( symbol = symbol ) ( group_symbols-symbol ) ).
    terminals = VALUE #( FOR symbol IN symbols WHERE ( table_line->type = lcl_symbol=>c_type-terminal ) ( CAST #( symbol ) ) ).
    non_terminals = VALUE #( FOR symbol IN symbols WHERE ( table_line->type = lcl_symbol=>c_type-non_terminal ) ( CAST #( symbol ) ) ).

  ENDMETHOD.


  METHOD create_item_set_transit_tables.

    transitions = NEW lcl_transitions( ).

    item_sets = NEW lcl_item_sets( )->append( NEW lcl_item_set( NEW lcl_items( VALUE #( ( NEW lcl_item(
                    type         = lcl_item=>c_type-kernel
                    rule         = start_rule
                    cursor_index = 1 ) ) ) ) ) ).

    LOOP AT item_sets->table INTO DATA(item_set).

      " add CLOSURE ITEMS
      DATA(non_terminals) = item_set->kernel_items->non_terminals_at_cursor_pos( ).
      LOOP AT non_terminals->table INTO DATA(non_terminal).
        DATA(new_closure_items) = NEW lcl_items( VALUE #(
            FOR rule IN rules WHERE ( table_line->lhs = non_terminal )
            ( NEW #( type         = lcl_item=>c_type-closure
                     rule         = rule
                     cursor_index = 1 ) ) ) ).
        item_set->add_closure_items( new_closure_items ).
        DATA(new_non_terminals) = new_closure_items->non_terminals_at_cursor_pos( ).
        DATA(new_non_terminals_2) = FILTER #( new_non_terminals->table EXCEPT IN non_terminals->table_by USING KEY by_object WHERE table_line = object ).
        non_terminals->append( new_non_terminals_2 ).
      ENDLOOP.

      " produce new item sets and transitions
      LOOP AT item_set->all_items->table INTO DATA(aux_item) WHERE table_line->symbol_at_cursor IS BOUND
          GROUP BY ( symbol_at_cursor = aux_item->symbol_at_cursor ) INTO DATA(group_of_items).
        DATA(kernel_items_of_next_item_set) = NEW lcl_items( VALUE lcl_items=>ty_items(
            FOR item IN GROUP group_of_items
            ( NEW lcl_item( type         = lcl_item=>c_type-kernel
                            rule         = item->rule
                            cursor_index = item->cursor_index + 1 ) ) ) ).
        DATA(next_item_set) = NEW lcl_item_set( kernel_items_of_next_item_set ).

        IF line_exists( item_sets->table_by[ KEY by_kernel_items COMPONENTS kernel_items_key = next_item_set->kernel_items_key ] ).
          next_item_set = item_sets->table_by[ KEY by_kernel_items COMPONENTS kernel_items_key = next_item_set->kernel_items_key ]-object.
        ELSE.
          item_sets->append( next_item_set ).
        ENDIF.
        transitions->append( VALUE #( item_set      = item_set
                                      symbol        = group_of_items-symbol_at_cursor
                                      next_item_set = next_item_set ) ).
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD create_first_sets.

    " create temporary first sets = one line per non terminal, and contains the rules where LHS = this non terminal and first element of RHS <> this non terminal
    " loop until temporary first sets is empty
    " - mark there is no change
    " - loop at temporary first sets
    "   - loop at rules of current temporary first set
    "     - if first RHS symbol is a terminal, append it to first set of current LHS, delete the rule from current temporary first set, and mark there was a change
    "     - if first RHS symbol is a non terminal, and it exists as a final first set, transfer all its terminals to current temporary first set, delete the rule from current temporary first set, and mark there was a change
    "   - if the current temporary first set has no more rule to analyze, append the current temporary first set to the final first sets, and delete the current temporary first set
    " - if there wasn't any change, it means that there is an "endless loop" -> raise error

    TYPES: BEGIN OF ty_temp_first_set,
             non_terminal TYPE REF TO lcl_non_terminal,
             rules        TYPE STANDARD TABLE OF REF TO lcl_rule WITH EMPTY KEY,
             terminals    TYPE lcl_first_set=>ty_terminals,
           END OF ty_temp_first_set,
           ty_temp_first_sets TYPE STANDARD TABLE OF ty_temp_first_set WITH EMPTY KEY.

    first_sets = NEW lcl_first_sets( ).

    DATA(temp_first_sets) = VALUE ty_temp_first_sets(
        FOR non_terminal IN non_terminals
        ( non_terminal = non_terminal
          rules        = VALUE #( FOR aux_rule IN rules WHERE ( table_line->lhs = non_terminal ) ( aux_rule ) )
          terminals    = VALUE #( ) ) ).
    LOOP AT temp_first_sets REFERENCE INTO DATA(temp_first_set).
      LOOP AT temp_first_set->rules INTO DATA(rule) WHERE table_line->rhs IS NOT INITIAL.
        IF rule->rhs[ 1 ] = rule->lhs.
          DELETE temp_first_set->rules USING KEY loop_key.
        ENDIF.
      ENDLOOP.
      IF temp_first_set->rules IS INITIAL.
        RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING error = zcx_ctxfreegram=>c_error-rules_circular_reference.
      ENDIF.
    ENDLOOP.


    WHILE temp_first_sets IS NOT INITIAL.

      DATA(change_done) = abap_false.

      LOOP AT temp_first_sets REFERENCE INTO temp_first_set.

        LOOP AT temp_first_set->rules INTO rule WHERE table_line->rhs IS NOT INITIAL.

          IF rule->rhs[ 1 ]->type = lcl_symbol=>c_type-terminal.
            COLLECT CAST lcl_terminal( rule->rhs[ 1 ] ) INTO temp_first_set->terminals.
            DELETE temp_first_set->rules USING KEY loop_key.
            change_done = abap_true.
          ELSE.
            DATA(first_set) = VALUE #( first_sets->table[ table_line->non_terminal = CAST lcl_non_terminal( rule->rhs[ 1 ] ) ] OPTIONAL ).
            IF first_set IS BOUND.
              LOOP AT first_set->terminals INTO DATA(terminal).
                COLLECT terminal INTO temp_first_set->terminals.
              ENDLOOP.
              DELETE temp_first_set->rules USING KEY loop_key.
              change_done = abap_true.
            ENDIF.
          ENDIF.

        ENDLOOP.

        IF temp_first_set->rules IS INITIAL.
          first_set = NEW lcl_first_set( non_terminal = temp_first_set->non_terminal terminals = temp_first_set->terminals ).
          first_sets->append( first_set ).
          DELETE temp_first_sets USING KEY loop_key.
        ENDIF.

      ENDLOOP.

      IF change_done = abap_false.
        RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING error = zcx_ctxfreegram=>c_error-rules_circular_reference.
      ENDIF.

    ENDWHILE.

  ENDMETHOD.


  METHOD create_follow_sets.
    DATA: current_follow_set TYPE REF TO lcl_follow_set.

    " table 2 (table for processing last RHS elements) = []
    " temporary follow sets = []
    " loop at rules
    " - loop at RHS elements
    "   - if it's not the last RHS element
    "     - if the next RHS element is a terminal, COLLECT it into the temporary follow set of current RHS element
    "     - else if the next RHS element is a non terminal, get its first set and COLLECT all terminals of this first set into the temporary follow set of current RHS element
    "   - else if it's the last RHS element
    "     - collect { target_follow_set = last RHS element , source_follow_set = LHS } into table 2
    "
    " final follow sets = { symbol = start , terminals = ["end of input"] }
    "
    " loop until table 2 is empty
    " - mark as not changed
    " - loop at table 2
    "   - if the source follow set of table 2 exists in symbol of final follow sets, then COLLECT all terminals of this follow set into the follow set of current RHS element, mark as changed, and delete the current line of table 2
    " - if nothing has changed then raise error
    "
    " loop at temporary follow sets
    " - COLLECT all terminals of this follow set into the corresponding final follow set, create it if needed
    TYPES : BEGIN OF ty_set_merge,
              key_source_follow_set TYPE REF TO lcl_non_terminal,
              key_target_follow_set TYPE REF TO lcl_symbol,
            END OF ty_set_merge,
            ty_set_merge_table TYPE STANDARD TABLE OF ty_set_merge WITH NON-UNIQUE KEY table_line.


    ASSERT first_sets IS BOUND.

    follow_sets = NEW lcl_follow_sets( ).

    DATA(temp_follow_sets) = NEW lcl_follow_sets( ).
    DATA(set_merge_table) = VALUE ty_set_merge_table( ).

    LOOP AT rules INTO DATA(rule).
      LOOP AT rule->rhs INTO DATA(current_rhs_element).
        DATA(tabix) = sy-tabix.
        current_follow_set = VALUE #( temp_follow_sets->table[ table_line->symbol = current_rhs_element ] OPTIONAL ).
        IF current_follow_set IS NOT BOUND.
          current_follow_set = NEW lcl_follow_set( symbol = current_rhs_element ).
          temp_follow_sets->append( current_follow_set ).
        ENDIF.
        IF tabix < lines( rule->rhs ).
          DATA(next_rhs_element) = rule->rhs[ tabix + 1 ].
          IF next_rhs_element->type = lcl_symbol=>c_type-terminal.
            current_follow_set->collect( CAST #( next_rhs_element ) ).
          ELSE.
            DATA(first_set) = first_sets->table[ table_line->non_terminal = CAST #( next_rhs_element ) ].
            LOOP AT first_set->terminals INTO DATA(terminal).
              current_follow_set->collect( terminal ).
            ENDLOOP.
          ENDIF.
        ELSE.
          COLLECT VALUE ty_set_merge( key_source_follow_set = rule->lhs key_target_follow_set = current_rhs_element ) INTO set_merge_table.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    DATA(start_follow_set) = NEW lcl_follow_set( symbol = start_rule->lhs ).
    start_follow_set->collect( terminal = end_of_input ).
    follow_sets->append( start_follow_set ).

    WHILE set_merge_table IS NOT INITIAL.
      DATA(change_done) = abap_false.
      LOOP AT set_merge_table REFERENCE INTO DATA(set_merge).
        DATA(source_follow_set) = VALUE #( follow_sets->table[ table_line->symbol = set_merge->key_source_follow_set ] OPTIONAL ).
        IF source_follow_set IS BOUND.
          DATA(target_follow_set) = VALUE #( follow_sets->table[ table_line->symbol = set_merge->key_target_follow_set ] OPTIONAL ).
          IF target_follow_set IS NOT BOUND.
            target_follow_set = NEW lcl_follow_set( symbol = set_merge->key_target_follow_set ).
            follow_sets->append( target_follow_set ).
          ENDIF.
          LOOP AT source_follow_set->terminals INTO terminal.
            target_follow_set->collect( terminal ).
          ENDLOOP.
          DATA(temp_follow_set) = VALUE #( temp_follow_sets->table[ table_line->symbol = set_merge->key_target_follow_set ] OPTIONAL ).
          IF temp_follow_set IS BOUND.
            LOOP AT temp_follow_set->terminals INTO terminal.
              target_follow_set->collect( terminal ).
            ENDLOOP.
          ENDIF.
          temp_follow_sets->remove( temp_follow_set ).
          change_done = abap_true.
          DELETE set_merge_table USING KEY loop_key.
        ENDIF.
      ENDLOOP.
      IF change_done = abap_false.
        RAISE EXCEPTION TYPE zcx_ctxfreegram EXPORTING error = zcx_ctxfreegram=>c_error-rules_circular_reference.
      ENDIF.
    ENDWHILE.

    LOOP AT temp_follow_sets->table INTO temp_follow_set.
*      DATA(follow_set) = VALUE #( follow_sets->table[ table_line->symbol = temp_follow_set->symbol ] OPTIONAL ).
*      IF follow_set IS NOT BOUND.
      follow_sets->append( temp_follow_set ).
*      ELSE.
*      LOOP AT temp_follow_set->terminals INTO terminal.
*        follow_set->collect( terminal ).
*      ENDLOOP.
*      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_first_and_follow_sets.


*    first_sets = NEW lcl_first_sets( ).
*    follow_sets = NEW lcl_follow_sets( ).
    DATA(process_once) = NEW lcl_ffs_process_once( ).
    REFRESH aut_first_set.
    REFRESH aut_follow_set.

    LOOP AT symbols INTO DATA(symbol) "ASSIGNING <ls_symbol2>
          WHERE table_line->type = lcl_symbol=>c_type-terminal
            OR  table_line->type = lcl_symbol=>c_type-non_terminal.

      IF symbol->type = lcl_symbol=>c_type-non_terminal.
        create_first_set( non_terminal = CAST #( symbol ) process_once = process_once ).
*        aut_first_set = VALUE #(
*          BASE aut_first_set
*          ( non_terminal = CAST #( symbol )
*            first_set    = create_first_set( non_terminal = CAST #( symbol ) process_once = process_once ) ) ).
      ENDIF.

      create_follow_set( symbol = symbol process_once = process_once ).
*      aut_follow_set = VALUE #(
*        BASE aut_follow_set
*        ( symbol     = symbol
*          follow_set = create_follow_set( symbol = symbol process_once = process_once ) ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD create_first_set.

    DATA: only_epsilons TYPE abap_bool.
    ASSIGN aut_first_set[ non_terminal = non_terminal ] TO FIELD-SYMBOL(<first_set>).
    IF sy-subrc = 0.
      symbols = <first_set>-first_set.
      RETURN.
    ENDIF.

    INSERT VALUE #( non_terminal = non_terminal first_set = NEW #( ) ) INTO TABLE aut_first_set ASSIGNING <first_set>.
    symbols = <first_set>-first_set.

    DATA(at_least_one_rule_epsilon) = abap_false.
    LOOP AT rules INTO DATA(rule)
          WHERE table_line->lhs = non_terminal.
      only_epsilons = abap_true.
      LOOP AT rule->rhs INTO DATA(symbol).
        IF symbol->type = lcl_symbol=>c_type-non_terminal.
          symbols->collect_many( create_first_set( non_terminal = CAST #( symbol ) process_once = process_once ) ).
        ELSEIF symbol = end_of_input.
          " Process next symbol in the RHS, its First Set will make the First Set of I_NONTERMINAL
          CONTINUE.
        ELSE. "terminal
          symbols->collect_one( symbol ).
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
      symbols->collect_many( create_follow_set( symbol = symbol process_once = process_once ) ).
    ENDIF.

  ENDMETHOD.


  METHOD create_follow_set.

    DATA: l_tabix       TYPE sytabix.
    ASSIGN aut_follow_set[ symbol = symbol ] TO FIELD-SYMBOL(<follow_set>).
    IF sy-subrc = 0.
      symbols = <follow_set>-follow_set.
      RETURN.
    ENDIF.

    INSERT VALUE #( symbol = symbol follow_set = NEW #( ) ) INTO TABLE aut_follow_set ASSIGNING <follow_set>.
    symbols = <follow_set>-follow_set.

    IF symbol->type = lcl_symbol=>c_type-non_terminal
          AND CAST lcl_non_terminal( symbol ) = start_rule->lhs.
      symbols->collect_one( end_of_input ).
    ELSE.

      LOOP AT rules INTO DATA(rule).
        " for all terminals and nonterminals of RHS, the FOLLOW SET must be determined
        " except for symbol "end of input".
        LOOP AT rule->rhs INTO DATA(rhs_symbol)
              WHERE table_line = symbol.
          IF sy-tabix = lines( rule->rhs ).
            " if symbol is at the end of RHS, then take the follow-set of LHS of the production rule
            symbols->collect_many( create_follow_set( symbol = rule->lhs process_once = process_once ) ).
          ELSE.
            " if symbol is NOT at the end of RHS, then based on the type of the next symbol:
            "   - if it's a terminal, add it to the follow-set
            "   - if it's a nonterminal, add its whole first-set to the follow-set
            l_tabix = sy-tabix + 1.
            READ TABLE rule->rhs INDEX l_tabix INTO DATA(symbol2).
            ASSERT sy-subrc = 0.
            IF symbol2->type = lcl_symbol=>c_type-terminal.
              symbols->collect_one( symbol2 ).
            ELSE.
              symbols->collect_many( create_first_set( non_terminal = CAST #( symbol2 ) process_once = process_once ) ).
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD render_item_sets.

    result = concat_lines_of( table = VALUE string_table(
        FOR item_set IN item_sets->table INDEX INTO i
        ( |* Item set { i }:\n| )
        ( LINES OF VALUE #(
            FOR item IN item_set->all_items->table
            WHERE ( table_line->rule->rhs IS NOT INITIAL )
            ( |** Item { COND #( WHEN item->type = item->c_type-kernel THEN `kernel` ELSE `closure` ) }: { item->rule->lhs->get_text( ) } = {
                COND #( WHEN item->rule->rhs IS NOT INITIAL THEN
                concat_lines_of( sep = ` ` table = VALUE string_table(
                FOR elem IN item->rule->rhs INDEX INTO j
                ( COND #( WHEN j = item->cursor_index THEN `◆ ` ) && elem->get_text( )
                && COND #( WHEN item->cursor_index > lines( item->rule->rhs ) AND j = lines( item->rule->rhs ) THEN ` ◆` ) ) ) ) ) }\n| ) ) ) ) ).

  ENDMETHOD.


  METHOD render_transitions.

    " Transitions are the list of next item sets for each couple { current item set , current symbol }. "1" is the starting item set (start rule):
    " (all numbers are item sets)
    " ┌─ start item set
    " ↓
    "    '*'  '+'  '0'  '1'  E  B  \n
    " 1            4    5    2  3  \n
    " 2  6    7                    \n
    " 3                            \n
    " 4                            \n
    " 5                            \n
    " 6            4    5       8  \n
    " 7            4    5       9  \n
    " 8                            \n
    " 9                            \n
    "
    " Example:
    " - start rule (item set 1) can be '0' or '1'.
    " - item sets 4 and 5 are reduced to rule B (cannot be seen)
    " - start rule then continues with B -> item set 3
    " - item set 3 is reduced to rule E (cannot be seen)
    " - start rule then continues with E -> item set 2
    " - item set 2 can be '*' or '+'.
    " - ...

    DATA ref_table TYPE REF TO data.
    FIELD-SYMBOLS <transitions> TYPE STANDARD TABLE.

    ref_table = create_render_table( ).
    ASSIGN ref_table->* TO <transitions>.

    " HEADING
    APPEND INITIAL LINE TO <transitions> ASSIGNING FIELD-SYMBOL(<transition>).
    LOOP AT symbols INTO DATA(symbol) WHERE table_line <> start_rule->lhs.
      DATA(compname) = |{ COND #( WHEN symbol->type = symbol->c_type-terminal THEN 'T' ELSE 'N' ) }{ sy-tabix }|.
      ASSIGN COMPONENT compname OF STRUCTURE <transition> TO FIELD-SYMBOL(<comp>).
      <comp> = symbol->get_text( ).
    ENDLOOP.

    " TRANSITIONS
    LOOP AT item_sets->table INTO DATA(item_set).
      DATA(item_set_index) = sy-tabix.

      APPEND INITIAL LINE TO <transitions> ASSIGNING <transition>.

      ASSIGN COMPONENT 'ITEMSET' OF STRUCTURE <transition> TO <comp>.
      ASSERT sy-subrc = 0.
      <comp> = |{ item_set_index }|.

      LOOP AT transitions->table REFERENCE INTO DATA(transition)
          WHERE item_set = item_set
            AND symbol IS BOUND.
        compname = |{ COND #( WHEN transition->symbol->type = transition->symbol->c_type-terminal THEN 'T' ELSE 'N' ) }{ line_index( symbols[ table_line = transition->symbol ] ) }|.
        ASSIGN COMPONENT compname OF STRUCTURE <transition> TO <comp>.
        ASSERT sy-subrc = 0.
        <comp> = |{ line_index( item_sets->table[ table_line = transition->next_item_set ] ) }|.
      ENDLOOP.

    ENDLOOP.

    result = render_table( <transitions> ).

  ENDMETHOD.


  METHOD render_first_sets.

    DATA ref_table TYPE REF TO data.
    FIELD-SYMBOLS <first_sets> TYPE STANDARD TABLE.

    DATA(components) = VALUE abap_component_tab(
            LET type_string   = cl_abap_elemdescr=>get_string( )
                max_terminals = REDUCE i( INIT max = 0 FOR aux_first_set IN first_sets->table NEXT max = nmax( val1 = max val2 = lines( aux_first_set->terminals ) ) )
            IN
        ( name = 'NON_TERMINAL' type = type_string )
        ( LINES OF VALUE #(
            FOR i = 1 WHILE i <= max_terminals
            ( name = |T_{ i }|
              type = type_string ) ) ) ).

    DATA(rtti_table) = cl_abap_tabledescr=>get( p_line_type = cl_abap_structdescr=>get( components ) ).

    CREATE DATA ref_table TYPE HANDLE rtti_table.
    ASSIGN ref_table->* TO <first_sets>.

    " HEADING
    APPEND INITIAL LINE TO <first_sets> ASSIGNING FIELD-SYMBOL(<first_set>).
    LOOP AT components ASSIGNING FIELD-SYMBOL(<component>).
      ASSIGN COMPONENT sy-tabix OF STRUCTURE <first_set> TO FIELD-SYMBOL(<comp>).
      ASSERT sy-subrc = 0.
      <comp> = <component>-name.
    ENDLOOP.

    " LINES
    LOOP AT first_sets->table INTO DATA(first_set).

      APPEND INITIAL LINE TO <first_sets> ASSIGNING <first_set>.

      ASSIGN COMPONENT 'NON_TERMINAL' OF STRUCTURE <first_set> TO <comp>.
      ASSERT sy-subrc = 0.
      <comp> = first_set->non_terminal->get_text( ).

      LOOP AT first_set->terminals INTO DATA(terminal).
        ASSIGN COMPONENT sy-tabix + 1 OF STRUCTURE <first_set> TO <comp>.
        ASSERT sy-subrc = 0.
        <comp> = terminal->get_text( ).
      ENDLOOP.

    ENDLOOP.

    result = render_table( <first_sets> ).

  ENDMETHOD.


  METHOD render_follow_sets.

    DATA ref_table TYPE REF TO data.
    FIELD-SYMBOLS <follow_sets> TYPE STANDARD TABLE.

    DATA(components) = VALUE abap_component_tab(
            LET type_string   = cl_abap_elemdescr=>get_string( )
                max_terminals = REDUCE i( INIT max = 0 FOR aux_follow_set IN follow_sets->table NEXT max = nmax( val1 = max val2 = lines( aux_follow_set->terminals ) ) )
            IN
        ( name = 'SYMBOL' type = type_string )
        ( LINES OF VALUE #(
            FOR i = 1 WHILE i <= max_terminals
            ( name = |T_{ i }|
              type = type_string ) ) ) ).

    DATA(rtti_table) = cl_abap_tabledescr=>get( p_line_type = cl_abap_structdescr=>get( components ) ).

    CREATE DATA ref_table TYPE HANDLE rtti_table.
    ASSIGN ref_table->* TO <follow_sets>.

    " HEADING
    APPEND INITIAL LINE TO <follow_sets> ASSIGNING FIELD-SYMBOL(<follow_set>).
    LOOP AT components ASSIGNING FIELD-SYMBOL(<component>).
      ASSIGN COMPONENT sy-tabix OF STRUCTURE <follow_set> TO FIELD-SYMBOL(<comp>).
      ASSERT sy-subrc = 0.
      <comp> = <component>-name.
    ENDLOOP.

    " LINES
    LOOP AT follow_sets->table INTO DATA(follow_set).

      APPEND INITIAL LINE TO <follow_sets> ASSIGNING <follow_set>.

      ASSIGN COMPONENT 'SYMBOL' OF STRUCTURE <follow_set> TO <comp>.
      ASSERT sy-subrc = 0.
      <comp> = follow_set->symbol->get_text( ).

      LOOP AT follow_set->terminals INTO DATA(terminal).
        ASSIGN COMPONENT sy-tabix + 1 OF STRUCTURE <follow_set> TO <comp>.
        ASSERT sy-subrc = 0.
        <comp> = terminal->get_text( ).
      ENDLOOP.

    ENDLOOP.

    result = render_table( <follow_sets> ).

  ENDMETHOD.


  METHOD create_render_table.

    DATA(components) = VALUE abap_component_tab(
            LET type = cl_abap_elemdescr=>get_string( ) IN
        ( name = 'ITEMSET' type = cl_abap_elemdescr=>get_string( ) )
        ( LINES OF COND #( WHEN terminals = abap_true THEN VALUE #(
            FOR terminal IN symbols
            WHERE ( table_line->type = lcl_symbol=>c_type-terminal )
            ( name = |T{ line_index( symbols[ table_line = terminal ] ) }|
              type = type ) ) ) )
        ( LINES OF COND #( WHEN nonterminals = abap_true THEN VALUE #(
            FOR non_terminal IN symbols
            WHERE ( table_line <> start_rule->lhs
                AND table_line->type = lcl_symbol=>c_type-non_terminal )
            ( name = |N{ line_index( symbols[ table_line = non_terminal ] ) }|
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

  METHOD factory.

    instance = NEW zcl_ctxfreegram2( ).

  ENDMETHOD.

  METHOD class_constructor.

  ENDMETHOD.

ENDCLASS.
