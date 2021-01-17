*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_symbol DEFINITION ABSTRACT.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM enum_type STRUCTURE c_type,
             terminal,
             non_terminal,
           END OF ENUM enum_type STRUCTURE c_type.
    METHODS get_text ABSTRACT
      RETURNING
        VALUE(text) TYPE string.
    DATA type TYPE enum_type.
ENDCLASS.

CLASS lcl_symbols DEFINITION.
  PUBLIC SECTION.
    METHODS collect_one
      IMPORTING
        symbol TYPE REF TO lcl_symbol.
    METHODS collect_many
      IMPORTING
        symbols TYPE REF TO lcl_symbols."zcl_ctxfreegram2=>ty_ut_symbol.
    DATA: symbols TYPE SORTED TABLE OF REF TO lcl_symbol WITH UNIQUE KEY table_line.
ENDCLASS.

CLASS lcl_terminal DEFINITION
    INHERITING FROM lcl_symbol.
  PUBLIC SECTION.
    INTERFACES zif_ctxfreegram_rule_term.
    METHODS constructor
      IMPORTING
        text TYPE string.
    METHODS get_text REDEFINITION.
    DATA text TYPE string READ-ONLY.
ENDCLASS.

CLASS lcl_non_terminal DEFINITION
    INHERITING FROM lcl_symbol.
  PUBLIC SECTION.
    INTERFACES zif_ctxfreegram_rule_nonterm.
    METHODS constructor
      IMPORTING
        name TYPE string.
    METHODS get_text REDEFINITION.
    DATA name TYPE string READ-ONLY.
ENDCLASS.

CLASS lcl_non_terminals DEFINITION.
  PUBLIC SECTION.
    TYPES: ty_non_terminals TYPE STANDARD TABLE OF REF TO lcl_non_terminal,
           BEGIN OF ty_non_terminal_by,
             name   TYPE string,
             object TYPE REF TO lcl_non_terminal,
           END OF ty_non_terminal_by,
           ty_non_terminals_by TYPE STANDARD TABLE OF ty_non_terminal_by
             WITH UNIQUE SORTED KEY by_name   COMPONENTS name
             WITH UNIQUE SORTED KEY by_object COMPONENTS object.
    METHODS append
      IMPORTING
        non_terminals TYPE ty_non_terminals
      RETURNING
        VALUE(fluent) TYPE REF TO lcl_non_terminals.
    DATA: table    TYPE ty_non_terminals READ-ONLY,
          table_by TYPE ty_non_terminals_by READ-ONLY.
ENDCLASS.

CLASS lcl_rule DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_ctxfreegram_rule.
    TYPES: ty_rhs TYPE STANDARD TABLE OF REF TO lcl_symbol WITH EMPTY KEY.
    DATA: lhs   TYPE REF TO lcl_non_terminal READ-ONLY,
          rhs   TYPE ty_rhs READ-ONLY,
          index TYPE i READ-ONLY.
    METHODS constructor
      IMPORTING
        lhs TYPE REF TO lcl_non_terminal
        rhs TYPE ty_rhs.
  PRIVATE SECTION.
    CLASS-DATA: last_rule_index TYPE i.
ENDCLASS.

CLASS lcl_item DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM enum_type STRUCTURE c_type,
             kernel,
             closure,
           END OF ENUM enum_type STRUCTURE c_type.
    METHODS constructor
      IMPORTING
        type         TYPE enum_type
        rule         TYPE REF TO lcl_rule
        cursor_index TYPE i.
    DATA:
      type             TYPE enum_type,
      cursor_index     TYPE i,
      rule             TYPE REF TO lcl_rule,
      symbol_at_cursor TYPE REF TO lcl_symbol.
ENDCLASS.

CLASS lcl_items DEFINITION.
  PUBLIC SECTION.
    TYPES ty_items TYPE STANDARD TABLE OF REF TO lcl_item WITH EMPTY KEY.
    METHODS constructor
      IMPORTING
        items TYPE ty_items OPTIONAL.
    METHODS non_terminals_at_cursor_pos
      RETURNING
        VALUE(non_terminals) TYPE REF TO lcl_non_terminals.
    METHODS append
      IMPORTING
        items         TYPE REF TO lcl_items
      RETURNING
        VALUE(fluent) TYPE REF TO lcl_items.
    DATA: table TYPE ty_items READ-ONLY.
ENDCLASS.

CLASS lcl_item_set DEFINITION.
  PUBLIC SECTION.
    TYPES : BEGIN OF ty_kernel_item,
              rule         TYPE REF TO lcl_rule,
              cursor_index TYPE i,
            END OF ty_kernel_item,
            ty_kernel_items TYPE STANDARD TABLE OF ty_kernel_item WITH EMPTY KEY.
    METHODS constructor
      IMPORTING
        kernel_items TYPE REF TO lcl_items.
    METHODS add_closure_items
      IMPORTING
        items TYPE REF TO lcl_items.
    DATA: kernel_items     TYPE REF TO lcl_items,
          all_items        TYPE REF TO lcl_items,
          kernel_items_key TYPE string.
ENDCLASS.

CLASS lcl_item_sets DEFINITION.
  PUBLIC SECTION.
    TYPES: ty_item_sets TYPE STANDARD TABLE OF REF TO lcl_item_set WITH EMPTY KEY,
           BEGIN OF ty_item_set_by,
             index            TYPE i,
             kernel_items_key TYPE string,
*             kernel_items TYPE lcl_item_set=>ty_kernel_items, "lcl_items=>ty_items, "
             object           TYPE REF TO lcl_item_set,
           END OF ty_item_set_by,
           ty_item_sets_by TYPE SORTED TABLE OF ty_item_set_by WITH UNIQUE KEY object
                WITH UNIQUE SORTED KEY by_kernel_items COMPONENTS kernel_items_key."kernel_items.
    METHODS append
      IMPORTING
        item_set      TYPE REF TO lcl_item_set
      RETURNING
        VALUE(fluent) TYPE REF TO lcl_item_sets.
    DATA: table    TYPE ty_item_sets READ-ONLY,
          table_by TYPE ty_item_sets_by READ-ONLY.
ENDCLASS.

CLASS lcl_transitions DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_transition,
             item_set      TYPE REF TO lcl_item_set,
             symbol        TYPE REF TO lcl_symbol,
             next_item_set TYPE REF TO lcl_item_set,
           END OF ty_transition,
           ty_transitions TYPE STANDARD TABLE OF ty_transition WITH EMPTY KEY.
    METHODS append
      IMPORTING
        transition    TYPE ty_transition
      RETURNING
        VALUE(fluent) TYPE REF TO lcl_transitions.
    DATA table TYPE ty_transitions READ-ONLY.
ENDCLASS.

CLASS lcl_ffs_process_once DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM enum_ffs_type STRUCTURE c_ffs_type,
             first_set,
             follow_set,
           END OF ENUM enum_ffs_type STRUCTURE c_ffs_type,
           BEGIN OF ty_symbol_processed,
             symbol   TYPE REF TO lcl_symbol,
             ffs_type TYPE enum_ffs_type,
           END OF ty_symbol_processed.
    METHODS check
      IMPORTING
        symbol                          TYPE REF TO lcl_symbol
        ffs_type                        TYPE enum_ffs_type
      RETURNING
        VALUE(processed_for_first_time) TYPE abap_bool.
    DATA symbols_processed TYPE SORTED TABLE OF ty_symbol_processed WITH UNIQUE KEY symbol ffs_type.
ENDCLASS.

CLASS lcl_first_set DEFINITION.
  PUBLIC SECTION.
    TYPES: ty_terminals TYPE SORTED TABLE OF REF TO lcl_terminal WITH UNIQUE KEY table_line.
    METHODS constructor
      IMPORTING
        non_terminal TYPE REF TO lcl_non_terminal
        terminals    TYPE ty_terminals.
    DATA: non_terminal TYPE REF TO lcl_non_terminal READ-ONLY,
          terminals    TYPE TABLE OF REF TO lcl_terminal READ-ONLY.
ENDCLASS.

CLASS lcl_first_sets DEFINITION.
  PUBLIC SECTION.
    METHODS append
      IMPORTING
        first_set TYPE REF TO lcl_first_set.
    DATA: table TYPE TABLE OF REF TO lcl_first_set READ-ONLY.
ENDCLASS.

CLASS lcl_follow_set DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        symbol TYPE REF TO lcl_symbol.
    METHODS COLLECT
      IMPORTING
        terminal TYPE REF TO lcl_terminal.
    DATA: symbol    TYPE REF TO lcl_symbol READ-ONLY,
          terminals TYPE TABLE OF REF TO lcl_terminal READ-ONLY.
ENDCLASS.

CLASS lcl_follow_sets DEFINITION.
  PUBLIC SECTION.
    METHODS append
      IMPORTING
        follow_set TYPE REF TO lcl_follow_set.
    METHODS remove
      IMPORTING
        follow_set TYPE REF TO lcl_follow_set.
    DATA: table TYPE TABLE OF REF TO lcl_follow_set READ-ONLY.
ENDCLASS.
