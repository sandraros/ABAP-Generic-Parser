INTERFACE zif_ctxfreegram_rule_elem
  PUBLIC .

  TYPES ty_elements TYPE STANDARD TABLE OF REF TO zif_ctxfreegram_rule_elem WITH EMPTY KEY.
  TYPES: BEGIN OF ENUM enum_type STRUCTURE c_type,
           terminal,
           terminal_regex,
           nonterminal,
           sequence,
         END OF ENUM enum_type STRUCTURE c_type.
  DATA type TYPE enum_type.
  METHODS get_elements
    RETURNING
      VALUE(elements) TYPE ty_elements.
  METHODS get_text
    RETURNING
      VALUE(result) TYPE string.

ENDINTERFACE.
