INTERFACE zif_ctxfreegram_lexer
  PUBLIC .

  TYPES: begin of ty_token,
           offset TYPE i,
           length type i,
         end of ty_token.
  "! <p class="shorttext synchronized" lang="en"></p>
  "!
  "! @parameter r_token | <p class="shorttext synchronized" lang="en"></p>
  "! @raising zcx_ctxfreegram | <p class="shorttext synchronized" lang="en"></p>
  METHODS get_next_token
    RETURNING
      VALUE(r_token) TYPE ty_token
    RAISING
      zcx_ctxfreegram.

ENDINTERFACE.
