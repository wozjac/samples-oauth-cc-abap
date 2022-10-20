" Sample code from the blog post https://jacekw.dev/blog/2022/oauth-from-abap-on-premise/
CLASS zcl_oauth_client DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

  PRIVATE SECTION.
    CLASS-DATA:
      out TYPE REF TO if_oo_adt_classrun_out.
    CLASS-METHODS:
      call_backend.
ENDCLASS.

CLASS zcl_oauth_client IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    zcl_oauth_client=>out = out.
    call_backend( ).
  ENDMETHOD.

  METHOD call_backend.
    cl_http_client=>create_by_destination(
     EXPORTING
       destination              = 'VERCEL_BACKEND_CC'
     IMPORTING
       client                   = DATA(http_client)
     EXCEPTIONS
       argument_not_found       = 1
       destination_not_found    = 2
       destination_no_authority = 3
       plugin_not_active        = 4
       OTHERS                   = 5 ).

    IF sy-subrc <> 0.
      out->write( |http_client create error { sy-subrc }| ).
      RETURN.
    ENDIF.

    " Use OAuth client to execute client credentials flow and enrich http client
    TRY.
        DATA(outh_client) = cl_oauth2_client=>create(
          i_profile = 'Z_AUTH0_CC_PROFILE'
          i_configuration = 'Z_AUTH0_CC_PROFILE' ).

        outh_client->set_token(
          io_http_client = http_client
          i_param_kind = if_oauth2_client=>c_param_kind_header_field ).

        " In case of token is not available or expired, run client_credentials flow
      CATCH cx_oa2c.
        TRY.
            outh_client->execute_cc_flow( ).

            outh_client->set_token(
              io_http_client = http_client
              i_param_kind = if_oauth2_client=>c_param_kind_header_field ).

          CATCH cx_root INTO DATA(exception).
            out->write( exception->get_text( ) ).
        ENDTRY.
    ENDTRY.

    " Configure the rest of your call
    http_client->request->set_header_field(
      name  = '~request_method'
      value = 'GET' ).

    http_client->send(
     EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2 ).

    IF sy-subrc <> 0.
      out->write( |http_client send error { sy-subrc }| ).
      http_client->get_last_error( IMPORTING message = DATA(message) ).
      out->write( message ).
      RETURN.
    ENDIF.

    http_client->receive(
      EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3 ).

    IF sy-subrc <> 0.
      out->write( |http_client receive error { sy-subrc }| ).
      http_client->get_last_error( IMPORTING message = message ).
      out->write( message ).
      RETURN.
    ENDIF.

    DATA(response) = http_client->response->get_cdata( ).
    out->write( response ).

  ENDMETHOD.
ENDCLASS.
