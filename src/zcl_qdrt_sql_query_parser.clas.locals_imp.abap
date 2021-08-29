*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_token_parser IMPLEMENTATION.

  METHOD constructor.
    me->tokens = tokens.
    token_count = lines( tokens ).
    current_index = 1.
    current_token = tokens[ current_index ].
  ENDMETHOD.

  METHOD update_from_current.
    tokens[ current_index ] = current_token.
  ENDMETHOD.


  METHOD is_next_token.

    CHECK has_next_token( ).

    DATA(token) = tokens[ current_index + 1 ]-value.

    IF next_token CS ','.
      result = token_matches( check_list     = next_token
                              token_to_check = token ).
    ELSE.
      result = xsdbool( token = next_token ).
    ENDIF.

  ENDMETHOD.

  METHOD is_previous_token.

    CHECK has_previous_token( ).

    DATA(token) = tokens[ current_index - 1 ]-value.

    IF previous_token CS ','.
      result = token_matches( check_list     = previous_token
                              token_to_check = token ).
    ELSE.
      result = xsdbool( token = previous_token ).
    ENDIF.

  ENDMETHOD.

  METHOD next_token.
    CHECK has_next_token( ).

    ADD 1 TO current_index.
    current_token = tokens[ current_index ].
  ENDMETHOD.

  METHOD has_next_token.
    result = xsdbool( current_index + 1 <= token_count ).
  ENDMETHOD.

  METHOD has_previous_token.
    result = xsdbool( current_index - 1 >= 1 ).
  ENDMETHOD.

  METHOD get_token.
    DATA: token_list  TYPE string_table,
          token_range TYPE RANGE OF string.

    DATA(start_index) = COND #( WHEN start_from_current = abap_true THEN current_index ELSE 1 ).

    IF token CS ','.
      SPLIT token AT ',' INTO TABLE token_list.
      token_range = VALUE #(
        LET i = 'I' eq = 'EQ' IN
        FOR l_token IN token_list ( sign = i option = eq low = l_token )
      ).
    ELSE.
      token_range = VALUE #(
        ( sign = 'I' option = 'EQ' low = token )
      ).
    ENDIF.

    LOOP AT tokens ASSIGNING FIELD-SYMBOL(<token>) FROM start_index WHERE value IN token_range.
      DATA(token_index) = sy-tabix.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      current_index = token_index.
      current_token = <token>.
      exists = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD set_index_to_first.
    current_index = 1.
    current_token = tokens[ 1 ].
  ENDMETHOD.

  METHOD delete_current.
    CHECK current_index > 0 AND current_index <= token_count.

    DELETE tokens INDEX current_index.

    token_count = lines( tokens ).
    IF current_index > token_count.
      current_index = token_count.
    ENDIF.

    current_token = tokens[ current_index ].
  ENDMETHOD.

  METHOD delete_next.
    CHECK has_next_token( ).

    DELETE tokens INDEX current_index + 1.
  ENDMETHOD.

  METHOD previous_token.
    CHECK current_index > 1.

    SUBTRACT 1 FROM current_index.
    current_token = tokens[ current_index ].
  ENDMETHOD.

  METHOD delete_previous.
    CHECK current_index - 1 >= 1.

    DELETE tokens INDEX current_index - 1.
    SUBTRACT 1 FROM current_index.
  ENDMETHOD.

  METHOD token_matches.
    DATA(token_check) = replace( val = check_list sub = ',' with = '|' occ = 0 ).
    token_check = |({ token_check })|.

    result = xsdbool( count( val = token_to_check regex = token_check  ) > 0 ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_query_param_parser IMPLEMENTATION.

  METHOD lif_statement_parser~parse.
    param = NEW #( ).

*.. First token is always the DATA token so jump immediately to second
    next_token( ).
    parse_name( ).
    parse_type( ).
    parse_length( ).
    parse_decimals( ).
    parse_value( ).

    result = param.
  ENDMETHOD.

  METHOD parse_name.
    IF current_token-value CS '('. " length declaration inside parenthesis
      DATA(left_paranthesis) = find( val = current_token-value sub = '(' ).
      DATA(right_parenthesis) = find( val = current_token-value sub = ')' ).
      param->name = substring( val = current_token-value len = left_paranthesis ).
      param->length = substring( val = current_token-value
                                 off = left_paranthesis + 1
                                 len = right_parenthesis - left_paranthesis - 1 ).
    ELSE.
      param->name = current_token-value.
    ENDIF.

    param->line_in_query = current_token-row.
  ENDMETHOD.

  METHOD parse_decimals.
    CHECK get_token( 'DECIMALS' ).
    next_token( ).

    param->decimals = current_token-value.

    data(val) = sana_tok_alias_def.

  ENDMETHOD.

  METHOD parse_length.
    CHECK param->length IS NOT INITIAL.

    IF get_token( 'LENGTH' ).
      next_token( ).
      param->length = current_token-value.
    ELSEIF param->length IS INITIAL AND
           param->type = cl_abap_typedescr=>typekind_char.
      param->length = 1.
    ENDIF.

  ENDMETHOD.

  METHOD parse_type.
    IF get_token( 'TYPE' ).
      IF is_next_token( 'RANGE' ).
*...... skip an extra token to set current token to type of range
        next_token( ).
        next_token( ).
        param->is_range = abap_true.
      ENDIF.
      next_token( ).
      param->type = current_token-value.
    ELSE.
*... no concrete type specified so the default type 'C' will be used
      param->type = cl_abap_typedescr=>typekind_char.
    ENDIF.
  ENDMETHOD.

  METHOD parse_value.
    CHECK: get_token( 'VALUE' ),
           is_next_token( 'IS' ) = abap_false.

    next_token( ).

    param->default_value = replace( val = current_token-value sub = `'` occ = 0 with = space ).
    param->default_value_raw = current_token-value.
  ENDMETHOD.


ENDCLASS.

CLASS lcl_query_token_simplifier IMPLEMENTATION.

  METHOD simplify.
    simplify_by_clause( clause     = 'ORDER'
                        simplified = zcl_qdrt_sql_query_parser=>c_keywords-order_by ).
    simplify_by_clause( clause     = 'GROUP'
                        simplified = zcl_qdrt_sql_query_parser=>c_keywords-group_by ).
    simplify_joins( ).
    simplify_conditions( ).
    rt_tokens = tokens.
  ENDMETHOD.


  METHOD simplify_by_clause.
    WHILE get_token( clause ).
      IF NOT is_next_token( 'BY' ).
        EXIT.
      ENDIF.

      current_token-value =
      current_token-value_no_modifier = simplified .

      update_from_current( ).
      delete_next( ).
    ENDWHILE.
  ENDMETHOD.


  METHOD simplify_joins.

    WHILE get_token( 'JOIN' ).
      previous_token( ).

      IF current_token-value = 'INNER'.
        current_token-value = zcl_qdrt_sql_query_parser=>c_keywords-inner_join.
        current_token-value_no_modifier = 'JOIN'.
        update_from_current( ).
        delete_next( ).
      ELSEIF current_token-value = 'OUTER'.
        delete_next( ).
        previous_token( ).
        delete_next( ).
        current_token-value = |{ current_token-value } OUTER JOIN|.
        current_token-value_no_modifier = 'JOIN'.
        update_from_current( ).
*..... Abbreviated form of outer join
      ELSEIF current_token-value = 'LEFT' OR
             current_token-value = 'RIGHT'.
        current_token-value = |{ current_token-value } OUTER JOIN|.
        current_token-value_no_modifier = 'JOIN'.
        update_from_current( ).
        delete_next( ).
      ELSEIF current_token-value = 'CROSS'.
        current_token-value = |{ current_token-value } JOIN|.
        current_token-value_no_modifier = 'JOIN'.
        update_from_current( ).
        delete_next( ).
      ELSE.
*...... This is also an inner join
        next_token( ).
        current_token-value = zcl_qdrt_sql_query_parser=>c_keywords-inner_join.
        current_token-value_no_modifier = 'JOIN'.
        update_from_current( ).
      ENDIF.

    ENDWHILE.
  ENDMETHOD.


  METHOD simplify_conditions.

    WHILE get_token( 'NULL,INITIAL' ).
      IF is_previous_token( 'NOT' ).
        delete_previous( ).
        delete_previous( ).
        current_token-value_no_modifier = current_token-value.
        current_token-value = |IS NOT { current_token-value }|.
        update_from_current( ).
      ELSE.
        delete_previous( ).
        current_token-value_no_modifier = current_token-value.
        current_token-value = |IS { current_token-value }|.
        update_from_current( ).
      ENDIF.
    ENDWHILE.

    WHILE get_token( token = 'NOT' start_from_current = abap_true ).

      next_token( ).

      IF current_token-value = 'EXISTS' OR
         current_token-value = 'IN' OR
         current_token-value = 'BETWEEN' OR
         current_token-value = 'LIKE'.
        current_token-value_no_modifier = current_token-value.
        current_token-value = |NOT { current_token-value }|.
        update_from_current( ).
        delete_previous( ).

      ENDIF.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.
