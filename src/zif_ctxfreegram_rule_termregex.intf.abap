INTERFACE zif_ctxfreegram_rule_termregex
  PUBLIC .

  INTERFACES zif_ctxfreegram_rule_elem.
  DATA regex TYPE string READ-ONLY.

  METHODS applies
    IMPORTING
      text TYPE csequence
    RETURNING
      VALUE(result) TYPE abap_bool.

ENDINTERFACE.
