" Sample code from the blog post https://jacekw.dev/blog/2022/oauth-from-abap-on-premise
CLASS zcl_oauth_direct DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    TYPES:
      BEGIN OF access_token,
        access_token  TYPE string,
        token_type    TYPE string,
        id_token      TYPE string,
        refresh_token TYPE string,
        expires_in    TYPE i,
        scope         TYPE string,
        jti           TYPE string,
      END OF access_token.

  PRIVATE SECTION.
    CLASS-DATA:
      out TYPE REF TO if_oo_adt_classrun_out.
    CLASS-METHODS:
      get_access_token RETURNING VALUE(result) TYPE access_token,
      call_backend IMPORTING access_token TYPE access_token.
ENDCLASS.

CLASS zcl_oauth_direct IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    zcl_oauth_direct=>out = out.

    " Step 1: get access token
    DATA(access_token) = get_access_token( ).

    " Step 2: use it to call the protected resource
    call_backend( access_token ).

  ENDMETHOD.

  METHOD get_access_token.
    cl_http_client=>create_by_url(
     EXPORTING
       url                = 'https://dev-ycxqrx2b.us.auth0.com/oauth/token'
       ssl_id             = 'ANONYM'
     IMPORTING
       client             = DATA(http_client)
     EXCEPTIONS
       argument_not_found = 1
       plugin_not_active  = 2
       internal_error     = 3
       pse_not_found      = 4
       pse_not_distrib    = 5
       pse_errors         = 6
       OTHERS             = 7 ).

    IF sy-subrc <> 0.
      out->write( |http_client create error { sy-subrc }| ).
      RETURN.
    ENDIF.

    http_client->request->set_header_field(
      name  = '~request_method'
      value = 'POST' ).

    http_client->request->set_header_field(
      name  = 'Content-Type'
      value = 'application/json' ).

    http_client->request->set_header_field(
      name  = 'Accept'
      value = 'application/json' ).

    DATA(request_body) = `{ "audience": "https://samples-oauth-cc-backend.vercel.app", "grant_type": "client_credentials" }`.

    http_client->request->set_cdata(
      data   = request_body
      offset = 0
      length = strlen( request_body ) ).

    http_client->request->set_authorization(
      auth_type = ihttp_auth_type_basic_auth
      username = 'xi9m2QU7wfGIsEEvyGBchhM066QBj59y'
      password = 'K0......................................i' ).

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

    DATA(http_status) = http_client->response->get_header_field( '~status_code' ).
    DATA(json_response) = http_client->response->get_cdata( ).
    out->write( http_status ).

    cl_fdt_json=>json_to_data(
      EXPORTING iv_json = json_response
      CHANGING ca_data = result ).
  ENDMETHOD.


  METHOD call_backend.
    cl_http_client=>create_by_url(
     EXPORTING
       url                = 'https://samples-oauth-cc-backend.vercel.app/authorized'
       ssl_id             = 'ANONYM'
     IMPORTING
       client             = DATA(http_client)
     EXCEPTIONS
       argument_not_found = 1
       plugin_not_active  = 2
       internal_error     = 3
       pse_not_found      = 4
       pse_not_distrib    = 5
       pse_errors         = 6
       OTHERS             = 7 ).

    IF sy-subrc <> 0.
      out->write( |http_client create error { sy-subrc }| ).
      RETURN.
    ENDIF.

    " Use the token as Bearer in the authorization header
    CONCATENATE 'Bearer' access_token-access_token INTO DATA(bearer_token) SEPARATED BY space.

    http_client->request->set_header_field(
      name  = 'Authorization'
      value = bearer_token ).

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
