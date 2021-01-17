INTERFACE zif_ctxfreegram_rule_nonterm
  PUBLIC .

  INTERFACES zif_ctxfreegram_rule_elem.
  ALIASES get_text FOR zif_ctxfreegram_rule_elem~get_text.
  DATA name TYPE string READ-ONLY.

ENDINTERFACE.
