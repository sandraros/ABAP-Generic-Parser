*"* use this source file for your ABAP unit test classes
CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.
    METHODS test2 FOR TESTING RAISING cx_static_check.
ENDCLASS.
CLASS ltc_main IMPLEMENTATION.

  METHOD test.

    DATA(factory) = zcl_ctxfreegram_factory=>create( ).
    DATA(start) = factory->new_nonterminal( 'start' ).
    DATA(e) = factory->new_nonterminal( 'E' ).
    DATA(b) = factory->new_nonterminal( 'B' ).
    DATA(star) = factory->new_terminal( '*' ).
    DATA(plus) = factory->new_terminal( '+' ).
    DATA(_0) = factory->new_terminal( '0' ).
    DATA(_1) = factory->new_terminal( '1' ).

    DATA(grammar) = zcl_ctxfreegram_grammar=>create(
        start_rule = start
        rules = VALUE #(
            ( lhs = start rhs = e )
            ( lhs = e     rhs = factory->new_sequence( VALUE #( ( e ) ( star ) ( b ) ) ) )
            ( lhs = e     rhs = factory->new_sequence( VALUE #( ( e ) ( plus ) ( b ) ) ) )
            ( lhs = e     rhs = b )
            ( lhs = b     rhs = _0 )
            ( lhs = b     rhs = _1 ) ) ).

*    DATA(item_sets) = grammar->render_item_sets( ).
*    DATA(transitions) = grammar->render_transitions( ).
*    DATA(action_goto_tables) = grammar->render_action_goto_tables( ).

    DATA(tokenizer) = zcl_ctxfreegram_tokenizer_1c=>create_from_string( '1+0' ).

    DATA(lexer) = zcl_ctxfreegram_lexer=>create(
        io_context_free_grammar = grammar
        record_trace            = abap_true ).

    lexer->parser->parse( tokenizer ).

*    DATA(parse_tree) = grammar->render_parse_tree( ).
    " cl_abap_unit_assert=>assert_equals( ACT = ? EXP = ? MSG = ? ).
  ENDMETHOD.

  METHOD test2.

*    DATA(factory) = NEW zcl_ctxfreegram_factory( ).
*
*    DATA(start) = factory->new_nonterminal( 'start' ).
*    DATA(start2) = factory->new_nonterminal( 'start2' ).
*    DATA(start3) = factory->new_nonterminal( 'start3' ).
*    DATA(a) = factory->new_nonterminal( 'a' ).
*    DATA(am) = factory->new_nonterminal( 'am' ).
*    DATA(anycharacter) = factory->new_nonterminal( 'anycharacter' ).
*    DATA(_a) = factory->new_terminal( 'a' ).
*    DATA(_m) = factory->new_terminal( 'm' ).
*    DATA(_anycharacter) = factory->new_terminal( '?' ).
*    DATA(grammar) = NEW zcl_ctxfreegram( start_rule = start rules = VALUE #(
*        ( lhs = start  rhs = start2 )
*        ( lhs = start2 rhs = factory->new_sequence( VALUE #( ( start3 ) ( start2 ) ) ) ) " TODO endless loop if order is start2 then start3 - how to avoid it?
*        ( lhs = start2 rhs = start3 )
*        ( lhs = start3 rhs = a )
*        ( lhs = start3 rhs = am )
*        ( lhs = start3 rhs = anycharacter )
*        ( lhs = a      rhs = _a )
*        ( lhs = am     rhs = factory->new_sequence( VALUE #( ( _a ) ( _m ) ) ) )
*        ( lhs = anycharacter rhs = _anycharacter ) ) ).
*
*    DATA(itemsets) = grammar->render_itemsets( ).
*    DATA(transitions) = grammar->render_transitions( ).
*    DATA(actions) = grammar->render_actions( ).
*    DATA(goto_table) = grammar->render_goto_table( ).
*
*    DATA(parser) = NEW zcl_ctxfreegram_lr_parser(
*        io_context_free_grammar = grammar
*        record_trace            = abap_true ).
*
*    parser->parse( text = '?a?am?' ).

  ENDMETHOD.

ENDCLASS.
