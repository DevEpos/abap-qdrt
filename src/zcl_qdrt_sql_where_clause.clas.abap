"! <p class="shorttext synchronized" lang="en">Where clause in SQL command</p>
CLASS zcl_qdrt_sql_where_clause DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          line_length         TYPE i DEFAULT 120
          default_line_offset TYPE i DEFAULT 4,
      add_word
        IMPORTING
          word          TYPE string
          word_length   TYPE i OPTIONAL
        RETURNING
          VALUE(result) TYPE REF TO zcl_qdrt_sql_where_clause,
      start_new_line
        IMPORTING
          word          TYPE string OPTIONAL
        RETURNING
          VALUE(result) TYPE REF TO zcl_qdrt_sql_where_clause,
      get_result
        RETURNING
          VALUE(result) TYPE string_table.
  PRIVATE SECTION.
    DATA: line_length         TYPE i,
          where_clause        TYPE string_table,
          current_line        TYPE string,
          current_offset      TYPE i,
          default_line_offset TYPE i.
ENDCLASS.



CLASS zcl_qdrt_sql_where_clause IMPLEMENTATION.


  METHOD constructor.
    me->line_length = line_length.
    me->default_line_offset = default_line_offset.
    me->current_offset = default_line_offset.
  ENDMETHOD.


  METHOD add_word.
    DATA: tmp_line TYPE c LENGTH 200.

    DATA(remaining) = line_length - current_offset.

    DATA(l_word_length) = COND i( WHEN word_length IS INITIAL THEN strlen( word ) ELSE word_length ).

    IF l_word_length < remaining.
      tmp_line = current_line.
      tmp_line+current_offset(l_word_length) = word.
      current_offset = current_offset + l_word_length + 1.  " Add blank character
      current_line = tmp_line.
    ELSE.
      where_clause = VALUE #( BASE where_clause ( current_line ) ).
      current_line = word.
      current_offset = l_word_length + 1. " Add blank character
    ENDIF.

    result = me.
  ENDMETHOD.


  METHOD start_new_line.
    IF current_line IS NOT INITIAL.
      where_clause = VALUE #( BASE where_clause ( current_line ) ).
      CLEAR current_line.
      current_offset = default_line_offset.

      IF word IS NOT INITIAL.
        current_line = word.
        DATA(new_line_length) = strlen( word ).

        IF new_line_length > default_line_offset.
          current_offset = new_line_length.

          IF current_line+current_offset = space.
            current_offset = current_offset + 1.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

    result = me.
  ENDMETHOD.


  METHOD get_result.
    start_new_line( ).
    result = where_clause.
  ENDMETHOD.


ENDCLASS.
