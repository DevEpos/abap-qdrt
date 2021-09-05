"! <p class="shorttext synchronized" lang="en">Group configuration</p>
INTERFACE zif_qdrt_aggregation_config
  PUBLIC .

  TYPES:
    BEGIN OF ty_aggregation_field,
      column_key TYPE fieldname,
    END OF ty_aggregation_field,

    ty_aggregation_fields TYPE STANDARD TABLE OF ty_aggregation_field WITH EMPTY KEY.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Returns order by clause for select</p>
    get_order_by_clause
      RETURNING
        VALUE(result) TYPE string_table,

    "! <p class="shorttext synchronized" lang="en">Returns having clause for select</p>
    get_having_clause
      RETURNING
        VALUE(result) TYPE string_table.
ENDINTERFACE.
