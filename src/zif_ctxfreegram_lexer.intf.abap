INTERFACE zif_ctxfreegram_lexer
  PUBLIC .

  INTERFACES zif_ctxfreegram_tokenizer.

  ALIASES ty_token FOR zif_ctxfreegram_tokenizer~ty_token.
  ALIASES get_next_token FOR zif_ctxfreegram_tokenizer~get_next_token.

ENDINTERFACE.
