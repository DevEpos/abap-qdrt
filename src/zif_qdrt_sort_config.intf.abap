"! <p class="shorttext synchronized" lang="en">Sorting configuration</p>
INTERFACE zif_qdrt_sort_config
  PUBLIC .

  TYPES:
    BEGIN OF ty_sort_field,
      field_name     TYPE fieldname,
      sort_direction TYPE string,
    END OF ty_sort_field,

    ty_sort_fields TYPE STANDARD TABLE OF ty_sort_field WITH EMPTY KEY.


  METHODS:
    "! <p class="shorttext synchronized" lang="en">Adds new Sort field</p>
    add_sort_field
      IMPORTING
        field_name     TYPE fieldname
        sort_direction TYPE string default zif_qdrt_c_sort_direction=>ascending,
    "! <p class="shorttext synchronized" lang="en">Returns order by clause</p>
    get_order_by_clause
      RETURNING
        VALUE(result) TYPE string_table,
    "! <p class="shorttext synchronized" lang="en">Returns 'X' if no sort field exists</p>
    is_empty
      RETURNING
        VALUE(result) TYPE abap_bool.
ENDINTERFACE.
