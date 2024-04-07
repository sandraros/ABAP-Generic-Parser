INTERFACE zif_ctxfreegram_token
  PUBLIC .

  DATA offset TYPE i READ-ONLY.
  DATA length TYPE i READ-ONLY.

  "! Information used in exception texts containing offset, length, etc.
  METHODS get_detailed_info
    RETURNING
      VALUE(result) TYPE string.
*    RAISING
*      zcx_ctxfreegram.

  "! Text of the token
  METHODS get_text
    RETURNING
      VALUE(result) TYPE string.
*    RAISING
*      zcx_ctxfreegram.

ENDINTERFACE.
