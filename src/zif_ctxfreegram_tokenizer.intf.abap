INTERFACE zif_ctxfreegram_tokenizer
  PUBLIC .

*  TYPES: begin of ty_token,
*           offset TYPE i,
*           length type i,
*         end of ty_token.
*
  METHODS get_next_token
    RETURNING
      VALUE(result) TYPE ref to zif_ctxfreegram_token
    RAISING
      zcx_ctxfreegram_tokenizer.

  METHODS is_next_token
    RETURNING
      VALUE(result) TYPE abap_bool.

ENDINTERFACE.
