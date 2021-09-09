"! <p class="shorttext synchronized" lang="en">Creates subroutine pool program for selection data from db</p>
CLASS zcl_qdrt_sql_selector DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates new SQL selection</p>
      create
        IMPORTING
          select_clause   TYPE string_table
          from_clause     TYPE string_table
          where_clause    TYPE string_table OPTIONAL
          order_by_clause TYPE string_table OPTIONAL
          offset          TYPE i OPTIONAL
          group_by_clause TYPE string_table OPTIONAL
          having_clause   TYPE string_table OPTIONAL
          max_size        TYPE i DEFAULT 200
        RETURNING
          VALUE(result)   TYPE REF TO zcl_qdrt_sql_selector.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Determine during active aggregation</p>
      determine_size_for_group_by
        IMPORTING
          query_result TYPE REF TO data
        RETURNING
          VALUE(size)  TYPE zqdrt_no_of_lines
        RAISING
          zcx_qdrt_selection_common,
      "! <p class="shorttext synchronized" lang="en">Determines the size of existing entries</p>
      determine_size
        RETURNING
          VALUE(result) TYPE zqdrt_no_of_lines
        RAISING
          zcx_qdrt_selection_common,
      "! <p class="shorttext synchronized" lang="en">Returns SQL String for current select</p>
      get_select_sql
        RETURNING
          VALUE(select_stmnt) TYPE string,
      "! <p class="shorttext synchronized" lang="en">Selects data</p>
      select_data
        EXPORTING
          VALUE(result) TYPE table
        RAISING
          zcx_qdrt_selection_common,
      "! <p class="shorttext synchronized" lang="en">Set maximum number of rows</p>
      set_max_rows
        IMPORTING
          max_rows TYPE i,
      "! <p class="shorttext synchronized" lang="en">Sets the offset for the select</p>
      set_offset
        IMPORTING
          offset TYPE i,
      "! <p class="shorttext synchronized" lang="en">Update from clause</p>
      update_from
        IMPORTING
          from_clause TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      "! List of Strings
      select_clause   TYPE string_table,
      "! List of Strings
      from_clause     TYPE string_table,
      "! List of Strings
      where_clause    TYPE string_table,
      "! List of Strings
      order_by_clause TYPE string_table,
      "! List of Strings
      group_by_clause TYPE string_table,
      having_clause   TYPE string_table,
      max_size        TYPE i,
      offset          TYPE i.

    METHODS:
      fill_having
        CHANGING
          lines TYPE string_table,
      "! <p class="shorttext synchronized" lang="en">Fill coding lines with from clause</p>
      fill_from
        CHANGING
          lines TYPE string_table,
      "! <p class="shorttext synchronized" lang="en">Fill coding lines with group by clause</p>
      fill_group_by
        CHANGING
          lines TYPE string_table,
      "! <p class="shorttext synchronized" lang="en">Fill coding lines with order by clause</p>
      fill_order_by
        CHANGING
          lines TYPE string_table,
      "! <p class="shorttext synchronized" lang="en">Fill coding lines with select clause</p>
      fill_select
        CHANGING
          lines TYPE string_table,
      "! <p class="shorttext synchronized" lang="en">Fill coding lines with where clause</p>
      fill_where
        CHANGING
          lines TYPE string_table,
      "! <p class="shorttext synchronized" lang="en">Determines existing line count with CTE</p>
      get_group_by_size_by_cte
        RETURNING
          VALUE(result) TYPE zqdrt_no_of_lines
        RAISING
          zcx_qdrt_sql_query_error,
      "! <p class="shorttext synchronized" lang="en">Creates count query with CTE</p>
      create_count_query_for_cte
        RETURNING
          VALUE(result) TYPE REF TO  zcl_qdrt_sql_query
        RAISING
          zcx_qdrt_sql_query_error.
ENDCLASS.



CLASS zcl_qdrt_sql_selector IMPLEMENTATION.


  METHOD create.
    result = NEW zcl_qdrt_sql_selector( ).

    result->select_clause   = select_clause.
    result->from_clause     = from_clause.
    result->having_clause   = having_clause.
    result->where_clause    = where_clause.
    result->order_by_clause = order_by_clause.
    result->group_by_clause = group_by_clause.
    result->max_size        = max_size.
  ENDMETHOD.


  METHOD determine_size.
    TRY.
        SELECT COUNT( * )
          FROM (from_clause)
          WHERE (where_clause)
          INTO @result.
      CATCH cx_root INTO DATA(error).
        RAISE EXCEPTION TYPE zcx_qdrt_selection_common
          EXPORTING
            previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD determine_size_for_group_by.
    FIELD-SYMBOLS: <query_result> TYPE table.

    IF sy-saprl > 751. " Common table expressions exist
      TRY.
          size = get_group_by_size_by_cte( ).
          RETURN.
        CATCH zcx_qdrt_sql_query_error ##NEEDED.
      ENDTRY.
    ENDIF.

    ASSIGN query_result->* TO <query_result>.

    TRY.
        SELECT (select_clause)
          FROM (from_clause)
          WHERE (where_clause)
          GROUP BY (group_by_clause)
          HAVING (having_clause)
          ORDER BY (order_by_clause)
          INTO CORRESPONDING FIELDS OF TABLE @<query_result>.

        size = lines( <query_result> ).
        CLEAR <query_result>.
      CATCH cx_root INTO DATA(sql_error).
        RAISE EXCEPTION TYPE zcx_qdrt_selection_common
          EXPORTING
            previous = sql_error.
    ENDTRY.
  ENDMETHOD.


  METHOD get_select_sql.
    DATA: sql_lines TYPE string_table.

    fill_select( CHANGING lines = sql_lines ).
    fill_from( CHANGING lines = sql_lines ).
    fill_where( CHANGING lines = sql_lines ).
    fill_group_by( CHANGING lines = sql_lines ).
    fill_having( CHANGING lines = sql_lines ).
    fill_order_by( CHANGING lines = sql_lines ).

    CONCATENATE LINES OF sql_lines INTO select_stmnt SEPARATED BY cl_abap_char_utilities=>cr_lf.
  ENDMETHOD.


  METHOD select_data.
    TRY.
        IF offset IS NOT INITIAL.
          SELECT (select_clause)
            FROM (from_clause)
            WHERE (where_clause)
            GROUP BY (group_by_clause)
            HAVING (having_clause)
            ORDER BY (order_by_clause)
            INTO CORRESPONDING FIELDS OF TABLE @result
            OFFSET @offset
            UP TO @max_size ROWS.
        ELSE.
          SELECT (select_clause)
            FROM (from_clause)
            WHERE (where_clause)
            GROUP BY (group_by_clause)
            HAVING (having_clause)
            ORDER BY (order_by_clause)
            INTO CORRESPONDING FIELDS OF TABLE @result
            UP TO @max_size ROWS.
        ENDIF.
      CATCH cx_root INTO DATA(sql_error).
        RAISE EXCEPTION TYPE zcx_qdrt_selection_common
          EXPORTING
            previous = sql_error.
    ENDTRY.

  ENDMETHOD.


  METHOD set_max_rows.
    max_size = max_rows.
  ENDMETHOD.


  METHOD set_offset.
    me->offset = offset.
  ENDMETHOD.


  METHOD update_from.
    me->from_clause = from_clause.
  ENDMETHOD.


  METHOD create_count_query_for_cte.
    DATA: sql_lines TYPE string_table,
          query     TYPE string.

    sql_lines = VALUE #(
      ( |WITH| )
      ( |  +group_select as (| ) ).

    fill_select( CHANGING lines = sql_lines ).
    fill_from( CHANGING lines = sql_lines ).
    fill_where( CHANGING lines = sql_lines ).
    fill_group_by( CHANGING lines = sql_lines ).
    fill_having( CHANGING lines = sql_lines ).
    sql_lines = VALUE #( BASE sql_lines
      ( |)| )
      ( |SELECT COUNT(*) FROM +group_select| ) ).
    CONCATENATE LINES OF sql_lines INTO query SEPARATED BY cl_abap_char_utilities=>cr_lf.

    result = zcl_qdrt_sql_query_factory=>parse_query( query ).
  ENDMETHOD.


  METHOD fill_from.
    DATA: line TYPE string.

    LOOP AT from_clause ASSIGNING FIELD-SYMBOL(<from>).
      CLEAR: line.
      IF sy-tabix = 1.
        line = |  FROM { <from> }|.
      ELSE.
        line = |       { <from> }|.
      ENDIF.

      lines = VALUE #( BASE lines ( line ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_group_by.
    DATA: line TYPE string.

    LOOP AT group_by_clause ASSIGNING FIELD-SYMBOL(<group_by>).
      CLEAR: line.
      IF sy-tabix = 1.
        line = |  GROUP BY { <group_by> }|.
      ELSE.
        line = |           { <group_by> }|.
      ENDIF.

      lines = VALUE #( BASE lines ( line ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_having.
    DATA: line TYPE string.

    LOOP AT having_clause ASSIGNING FIELD-SYMBOL(<having>).
      CLEAR: line.
      IF sy-tabix = 1.
        line = |  HAVING { <having> }|.
      ELSE.
        line = |         { <having> }|.
      ENDIF.

      lines = VALUE #( BASE lines ( line ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_order_by.
    DATA: line TYPE string.

    LOOP AT order_by_clause ASSIGNING FIELD-SYMBOL(<order_by>).
      CLEAR: line.
      IF sy-tabix = 1.
        line = |  ORDER BY { <order_by> }|.
      ELSE.
        line = |           { <order_by> }|.
      ENDIF.

      lines = VALUE #( BASE lines ( line ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_select.
    DATA: line TYPE string.

    LOOP AT select_clause ASSIGNING FIELD-SYMBOL(<select>).
      CLEAR: line.
      IF sy-tabix = 1.
        line = |SELECT { <select> }|.
      ELSE.
        line = |       { <select> }|.
      ENDIF.

      lines = VALUE #( BASE lines ( line ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_where.
    DATA: line TYPE string.

    LOOP AT where_clause ASSIGNING FIELD-SYMBOL(<where>).
      CLEAR: line.
      IF sy-tabix = 1.
        line = |WHERE { <where> }|.
      ELSE.
        line = |   { <where> }|.
      ENDIF.

      lines = VALUE #( BASE lines ( line ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_group_by_size_by_cte.

    DATA(count_query) = create_count_query_for_cte( ).
    IF count_query IS INITIAL.
      RETURN.
    ENDIF.

    count_query->get_result( max_size ).
    count_query->get_single_value_result( IMPORTING result = result ).
  ENDMETHOD.


ENDCLASS.
