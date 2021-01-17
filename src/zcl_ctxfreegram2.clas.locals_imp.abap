*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_symbols IMPLEMENTATION.
  METHOD collect_one.
    COLLECT symbol INTO symbols.
  ENDMETHOD.
  METHOD collect_many.
    LOOP AT symbols->symbols INTO DATA(symbol).
      COLLECT symbol INTO me->symbols.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_terminal IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->text = text.
    type = c_type-terminal.
  ENDMETHOD.
  METHOD get_text.
    text = |'{ me->text }'|.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_non_terminal IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->name = name.
    type = c_type-non_terminal.
  ENDMETHOD.
  METHOD get_text.
    text = name.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_non_terminals IMPLEMENTATION.
  METHOD append.
    APPEND LINES OF non_terminals TO table.
    table_by = VALUE #(
        FOR non_terminal IN non_terminals
        ( name   = non_terminal->name
          object = non_terminal ) ).
    fluent = me.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_rule IMPLEMENTATION.
  METHOD constructor.
    me->lhs = lhs.
    me->rhs = rhs.
    last_rule_index = last_rule_index + 1.
    index = last_rule_index.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_item IMPLEMENTATION.
  METHOD constructor.
    me->type = type.
    me->rule = rule.
    me->cursor_index = cursor_index.
    symbol_at_cursor = VALUE #( rule->rhs[ cursor_index ] OPTIONAL ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_items IMPLEMENTATION.
  METHOD constructor.
    table = items.
  ENDMETHOD.
  METHOD non_terminals_at_cursor_pos.
    DATA symbols TYPE TABLE OF REF TO lcl_symbol.
    symbols = VALUE #(
        FOR item IN table
        WHERE ( table_line->symbol_at_cursor IS BOUND )
        ( item->symbol_at_cursor ) ).
    DELETE symbols WHERE table_line->type = lcl_symbol=>c_type-terminal.
    SORT symbols BY table_line.
    DELETE ADJACENT DUPLICATES FROM symbols COMPARING table_line.
    non_terminals  = NEW lcl_non_terminals( )->append( VALUE #(
        FOR symbol IN symbols
        ( CAST #( symbol ) ) ) ).
  ENDMETHOD.
  METHOD append.
    APPEND LINES OF items->table TO table.
    fluent = me.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_item_set IMPLEMENTATION.
  METHOD constructor.
    me->kernel_items = NEW lcl_items( )->append( kernel_items ).
    kernel_items_key = concat_lines_of( sep = ';' table = VALUE string_table(
        FOR kernel_item IN kernel_items->table
        ( |{ kernel_item->rule->index },{ kernel_item->cursor_index }| ) ) ).
    all_items = NEW lcl_items( )->append( kernel_items ).
  ENDMETHOD.
  METHOD add_closure_items.
    all_items->append( items ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_item_sets IMPLEMENTATION.
  METHOD append.
    APPEND item_set TO table.
    APPEND VALUE ty_item_set_by(
            index            = lines( table )
            kernel_items_key = item_set->kernel_items_key
*            kernel_items     = item_set->kernel_items
            object           = item_set
          ) TO table_by.
    fluent = me.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_transitions IMPLEMENTATION.
  METHOD append.
    APPEND transition TO table.
    fluent = me.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_ffs_process_once IMPLEMENTATION.
  METHOD check.
    " ignore a request already processed (avoid recursive)
    IF line_exists( symbols_processed[ symbol = symbol ffs_type = ffs_type ] ).
      processed_for_first_time = abap_false.
      EXIT.
    ENDIF.
    INSERT VALUE #( symbol = symbol ffs_type = ffs_type ) INTO TABLE symbols_processed.
    processed_for_first_time = abap_true.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_first_set IMPLEMENTATION.
  METHOD constructor.
    me->non_terminal = non_terminal.
    me->terminals = terminals.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_first_sets IMPLEMENTATION.
  METHOD append.
    append first_set to table.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_follow_set IMPLEMENTATION.
  METHOD constructor.
    me->symbol = symbol.
  ENDMETHOD.
  METHOD COLLECT.
COLLECT terminal INTO terminals.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_follow_sets IMPLEMENTATION.
  METHOD append.
    append follow_set to table.
  ENDMETHOD.
  METHOD remove.
    delete table where table_line = follow_set.
  ENDMETHOD.
ENDCLASS.
