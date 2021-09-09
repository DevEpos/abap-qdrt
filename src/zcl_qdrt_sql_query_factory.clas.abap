"! <p class="shorttext synchronized" lang="en">Factory for SQL queries</p>
CLASS zcl_qdrt_sql_query_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates query instance from SQL query string</p>
      parse_query
        IMPORTING
          query         TYPE string
        RETURNING
          VALUE(result) TYPE REF TO zcl_qdrt_sql_query
        RAISING
          zcx_qdrt_sql_query_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS:
    "! <p class="shorttext synchronized" lang="en">Event handler for Query Data request</p>
      on_request_result
        FOR EVENT request_result OF zcl_qdrt_sql_query
        IMPORTING
          max_rows
          sender.
ENDCLASS.



CLASS zcl_qdrt_sql_query_factory IMPLEMENTATION.


  METHOD parse_query.
    result = NEW zcl_qdrt_sql_query_parser( query )->parse( ).
    SET HANDLER:
      on_request_result FOR result.
  ENDMETHOD.


  METHOD on_request_result.
    DATA(query_executor) = NEW zcl_qdrt_sql_query_executor( sender ).
    TRY.
        DATA(query_result) = query_executor->execute_select( max_rows ).
        sender->set_result( query_result ).
      CATCH zcx_qdrt_sql_query_error INTO DATA(sql_error).
        sender->set_result( VALUE #( message = sql_error->get_text( ) ) ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
