#define LNLYLIB_API_IMPL
#include "net/http.h"

#define L_HTTP_MALLOC_TYPE(E, type) L_MALLOC_TYPE(E, type)
#define L_HTTP_MFREE(E, p) l_mfree(E, p)

typedef struct {
  l_service_access_point sap;
  l_socket_data_service* data_service;
} l_http_accept_service;

static l_bool l_http_accept_service_on_create(lnlylib_env* E);
static void l_http_accept_service_on_destroy(lnlylib_env* E);
static void l_http_accept_service_proc(lnlylib_env* E);

static l_service_callback
l_http_accept_service_callback = {
  l_http_accept_service_on_create,
  l_http_accept_service_on_destroy,
  l_http_accept_service_proc
};

static l_bool
l_http_accept_service_on_create(lnlylib_env* E)
{
  l_http_accept_service* accept_service = L_HTTP_MALLOC_TYPE(E, l_http_accept_service);
  return l_set_service_udata(accept_service);
}

static void
l_http_accept_service_on_destroy(lnlylib_env* E)
{
  L_HTTTP_MFREE(E, l_current_service_udata(E));
}

static void
l_http_accept_service_access_proc(lnlylib_env* E)
{
  l_message* msg = l_current_message(E);
  switch (l_message_id(msg)) {
  case L_MSG_SOCK_READY_NTF:
    break;
  case L_MSG_SOCK_READ_RSP:
    break;
  case L_MSG_SOCK_WRITE_RSP:
    break;
  case L_MSG_SOCK_BAD_STATE_NTF:
    break;
  default:
    break;
  }
}

static void
l_http_accept_service_proc(lnlylib_env* E)
{
  l_message* msg = 0;
  l_http_accept_service* accept_service = 0;

  accept_service = L_CURRENT_SERVICE_UDATA(E, l_http_accept_service);
  msg = l_current_message(E);

  if (l_message_id(msg) == L_MSG_SUBSRVC_CREATE_RSP) {
    l_subsrvc_create_rsp* rsp = L_MESSAGE_DATA(msg, l_subsrvc_create_rsp);
    switch (rsp.creation_ctx) {
    case 0:
      accept_service->sap.peer_svid = rsp.srvc_id;
      accept_service->sap.peer_apid = 0;
      accept_service->sap.access_proc = l_http_accept_service_access_proc;
      accept_service->data_service = (l_socket_data_service*)rsp.srvc_ud;
      break;
    default:
      break;
    }
  } else {
    if (l_message_from(msg) == accept_service->sap.peer_svid) {
      accept_service->sap.access_proc(E);
    } else {
      l_loge_1(E, "message from unknown service %svid", lx(l_message_from(msg)));
    }
  }
}

static l_bool
l_http_client_service_on_create(lnlylib_env* E)
{
}

static void
l_http_client_service_on_destroy(lnlylib_env* E)
{
}

static void
l_http_client_service_porc(lnlylib_env* E)
{
}

