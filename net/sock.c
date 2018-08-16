#define LNLYLIB_API_IMPL
#include "net/sock.h"

#if 0
l_socket_read(l_ulong sock_srvc)
l_socket_write(l_ulong sock_srvc)
l_socket_recover(l_ulong sock_srvc)
l_socket_shutdown(l_ulong sock_srvc)
#endif

typedef struct {
  l_service_callback* inconn_cb;
  l_dqueue connq;
  l_dqueue freeq;
  l_umedit conns;
  l_ushort local_port;
  l_byte local_ip[64];
} l_socket_listen_svud;

static void* l_socket_listen_service_on_create(lnlylib_env* E);
static void l_socket_listen_service_on_destroy(lnlylib_env* E);
static void l_socket_listen_service_proc(lnlylib_env* E);

static l_service_define
l_socket_listen_service = {
  "lonely-socket-listen-service",
  l_socket_listen_service_on_create,
  l_socket_listen_service_on_destroy,
  l_socket_listen_service_proc
};

L_EXTERN void
l_create_listen_service(lnlylib_env* E, const void* local_ip, l_ushort local_port, l_service_callback* conn_cb)
{
  l_create_service(E, L_LISTEN_SERVICE(ip, port, l_socket_listen_service), conn_cb);
}

L_EXTERN void
l_stop_listen_service(lnlylib_env* E)
{
  l_send_message(E, l_current_svfr(E), L_MSG_SOCK_STOP_LISTNER, 0, 0);
}

#if 0
static void
l_create_connect_service(lnlylib_env* E, const void* remote_ip, l_ushort remote_port)
{
  l_socket_data_svud* data_svud = 0;
}
#endif

typedef struct {
  l_ushort local_port;
  l_ushort remote_port;
  const l_byte* local_ip;
  const l_byte* remote_ip;
} l_socket_info_data;

typedef struct {
  l_linknode node;
  l_squeue wrmq;
  l_squeue rdmq;
  l_bool outconn;
  l_ulong user_svid;
  l_socket_info_data info;
  l_byte rmt_ip[64];
} l_socket_data_svud;

static void
l_socket_data_svud_init(l_socket_data_svud* s)
{
}

static void
l_socket_date_svud_free(l_socket_data_svud* s)
{
}

static void* l_socket_data_service_on_create(lnlylib_env* E);
static void l_socket_data_service_on_destroy(lnlylib_env* E);
static void l_socket_data_service_proc(lnlylib_env* E);

static l_service_define
l_socket_data_service = {
  "lonely-socket-data-service",
  l_socket_data_service_on_create,
  l_socket_data_service_on_destroy,
  l_socket_data_service_proc
};

static void*
l_socket_listen_service_on_create(lnlylib_env* E)
{
  l_socket_listen_svud* listen = 0;
  l_sockaddr local;

  listen = L_MALLOC_TYPE(E, l_socket_listen_svud);

  listen->inconn_cb = (l_service_callback*)l_current_svud(E);
  listen->conns = 0;
  l_dqueue_init(&listen->connq);
  l_dqueue_init(&listen->freeq);

  local = l_sockaddr_local(l_current_evhd(E));
  listen->local_port = l_sockaddr_port(&local);
  listen->local_ip = l_sockaddr_getip(&local);

  l_set_current_svud(E, listen);
  return 0;
}

static void
l_socket_listen_service_on_destroy(lnlylib_env* E)
{
  l_socket_listen_svud* data = 0;
  data = (l_socket_listen_svud*)l_current_svud(E);
  L_MFREE(E, data);
}

static void
l_socket_listen_service_accept_conn(void* ud, l_socketconn* conn)
{
  lnlylib_env* E = (lnlylib_env*)ud;
  l_sockaddr* remote = 0;
  l_sbuf64 rmt_ip;
  l_ushort rmt_port;
  int accept = true;

  remote = &conn->remote;
  rmt_port = l_sockaddr_port(remote);
  rmt_ip = l_sockaddr_getip(remote);

  /* TODO: do remote address filter here */

  if (accept) {
    l_socket_listen_svud* listen = 0;
    l_socket_data_svud* data_svud = 0;

    listen = (l_socket_listen_svud*)l_current_svud(E);
    data_svud = (l_socket_data_svud*)l_dqueue_pop(&listen->freeq);
    if (data_svud == 0) {
      data_svud = L_MALLOC_TYPE(E, l_socket_data_svud);
    }

    l_zero_n(data_svud, sizeof(l_socket_data_svud));

    data_svud->info.local_port = listen->local_port;
    data_svud->info.local_ip = listen->local_ip;
    data_svud->info.remote_port = rmt_port;
    data_svud->info.remote_ip = data_svud->rmt_ip;
    data_svud->rmt_ip = rmt_ip;

    l_create_service(E, L_USEHDL_SERVICE(conn->sock, l_socket_data_service), data_svud);
  }
}

static void
l_socket_listen_service_proc(lnlylib_env* E)
{
  l_umedit mgid = 0;
  l_service* S = l_current_service(E);
  l_message* MSG = l_current_message(E);
  l_socket_listen_svud* listen = 0;

  listen = (l_socket_listen_svud*)l_current_svud(E);

  switch (l_message_mgid(MSG)) {
  case L_MSG_SOCK_ACCEPT_IND: /* from master */
    l_socket_accept(l_service_evhd(S), l_socket_listen_service_accept_conn, E);
    break;
  case L_MSG_SUBSRVC_CREATE_RSP: {
    l_subsrvc_create_rsp* rsp = 0;
    l_socket_data_svud* data_svud = 0;
    rsp = (l_subsrvc_create_rsp*)l_message_mgdt(MSG).p;
    data_svud = (l_socket_data_svud*)rsp->svud;
    if (rsp->svid) {
      listen->conns += 1;
      l_dqueue_push(&listen->connq, &data_svud->node);

      l_send_message(E, rsp->svid, L_MSG_SOCK_CONN_NTF, 0, 0);
    } else {
      l_dqueue_push(&listen->freeq, &data_svud->node);
    }}
    break;
  case L_MSG_SOCK_DISC_NTF:
  case L_MSG_SOCK_ERROR_NTF:
    break;
  case L_MSG_SOCK_DISC_CMD: { /* from socket data service */
    l_socket_data_svud* data_svud = 0;
    data_svud = ((l_sock_disc_cmd*)l_message_mgdt(MSG).p)->data_svud;
    l_socket_data_svud_free(data_svud);
    listen->conns -= 1;
    l_dqueue_remove(&listen->connq, &data_svud->node);
    l_dqueue_push(&listen->freeq, &data_svud->node);
    l_stop_dest_service(E, l_message_frid(MSG));
    break;
  case L_MSG_SERVICE_EXIT_REQ:
    break;
  case L_MSG_SERVICE_RESTART_REQ:
    break;
  default:
    break;
  }
}

/** socket inconn service **/

static void*
l_socket_inconn_service_on_create(lnlylib_env* E)
{
  L_UNUSED(E); /* nothing to do */
  return 0;
}

static void
l_socket_inconn_service_on_destroy(lnlylib_env* E)
{
  l_socket_data_svud* data_svud = 0;
  data_svud = (l_socket_data_svud*)l_current_svud(E);
  if (data_svud->outconn) {
    l_socket_data_svud_free(data_svud);
    L_MFREE(data_svud);
  }
}

static void
l_socket_inconn_service_proc(lnlylib_env* E)
{
  /** related messages **
  REQ is sent from client to server and requires server RSP. CMD is sent from client to server, no response.
  IND is sent from server to client and requires client CNF, NTF is sent from server to client, no response.
  (1) socket user service
  [RX] L_MSG_SOCK_READY_NTF (all these are freom socket data txrx service)
       L_MSG_SOCK_DATA_RX_RSP
       L_MSG_SOCK_DATA_TX_RSP
       L_MSG_SOCK_ERROR_NTF
       L_MSG_SOCK_RECOVER_RSP
  [TX] L_MSG_SOCK_DATA_RX_REQ     <- l_socket_read(sock_srvc)
       L_MSG_SOCK_DATA_TX_REQ     <- l_socket_write(sock_svrc)
       L_MSG_SOCK_RECOVER_REQ     <- l_socket_recover(sock_srvc)
       L_MSG_SOCK_CLOSE_CMD       <- l_socket_close(sock_srvc)
       L_MSG_SOCK_STOP_SERVER_CMD <- l_stop_listen_server(E)
  (2) socket data txrx service
  [RX] L_MSG_SOCK_CONN_NTF (from listen server after incoming connection accepted)
       L_MSG_SOCK_CONN_RSP (from master after initiated connection established)
       L_MSG_SOCK_READY_TX (these are common socket messages from master)
       L_MSG_SOCK_READY_RX
       L_MSG_SOCK_DISC_NTF
       L_MSG_SOCK_ERROR_NTF
       L_MSG_SOCK_DATA_TX_REQ (these are from user service)
       L_MSG_SOCK_DATA_RX_REQ
       L_MSG_SOCK_RECOVER_REQ
       L_MSG_SOCK_CLOSE_CMD
       L_MSG_SOCK_STOP_SERVER_CMD
  [TX] L_MSG_SOCK_DISC_CMD (these are sent to listen server)
       L_MSG_SOCK_STOP_SERVER_CMD
       L_MSG_SOCK_READY_NTF (these are sent to user service)
       L_MSG_SOCK_ERROR_NTF
       L_MSG_SOCK_DATA_TX_RSP
       L_MSG_SOCK_DATA_RX_RSP
       L_MSG_SOCK_RECOVER_RSP
  (3) socket listen server service
  [RX] L_MSG_SOCK_ACCEPT_IND (from master when a connection incoming)
       L_MSG_SOCK_ERROR_NTF (from master when socket error)
  ----
  flow for incoming socket connection
       1. user pass inconn cb to service (3) via svud on create
       2. service (3) get inconn cb and replace svud to its real svud on create
       3. when (3) received L_MSG_SOCK_ACCEPT_IND, it creates a service (2) for each connection incoming
       4. when the service created success, (3) send L_MSG_SOCK_CONN_NTF to it
       5. when service (2) received L_MSG_SOCK_CONN_NTF, it create a service (1) to handle user logic (i.e. inconn cb)
       6. if the user service created success, (2) send L_MSG_SOCK_READY_NTF to it
       7. if created failed, (2) is no need to exist, send L_MSG_SOCK_DISC_CMD to (3) to release connection and destroy itself
       8. now, service (1) can tx/rx socket data via socket service (2), and if needed, (1) can also create outconn socket services
  **/

  l_message* MSG = E->MSG;
  l_service* S = E->S;
  l_socket_inconn_svud* svud = 0;

  l_message* msg = 0;
  l_byte* data = 0;
  l_umedit size = 0;
  l_umedit done = 0;

  svud = (l_socket_inconn_svud*)E->svud;

  switch (MSG->mssg_id) {
  case L_MSG_SOCK_CONN_NTF: /* from listen service, a incoming connection established */
    l_socket_data_svud* svud = 0;
    svud = (l_socket_inconn_svud*)l_current_svud(E);
    l_create_service(E, L_SERVICE(svud->listen_svud->inconn_cb), &svud->info);
    break;
  case L_MSG_SUBSRVC_CREATE_RSP: {
    l_subsrvc_create_rsp* rsp = 0;
    rsp = (l_subsrvc_create_rsp*)l_message_mgdt(MSG).p;
    if (rsp->svid) {
      l_sock_ready_ntf ntf;
      svud->user_svid = rsp->svid;
      ntf.sock_srvc = l_service_svid(S);
      l_send_message(E, svud->user_svid, L_MSG_SOCK_READY_NTF, &ntf, sizeof(l_sock_ready_ntf));
    } else {
      l_sock_disc_cmd cmd;
      cmd.data_service = svud;
      l_send_message(E, l_service_svfr(S), L_MSG_SOCK_DISC_CMD, &cmd, sizeof(l_sock_disc_cmd));
    }}
    break;
  case L_MSG_SOCK_CONN_RSP: { /* from master, initiated connection established */
    l_socket_data_svud* data_svud = 0;
    l_sock_ready_ntf ntf;

    /* TODO - complete the connection first */

    data_svud = L_MALLOC_TYPE(l_socket_data_svud);
    l_zero_n(data_svud, sizeof(l_socket_data_svud));
    data_svud->info.local_port = listen->local_port;
    data_svud->info.local_ip = listen->local_ip;
    data_svud->info.remote_port = rmt_port;
    data_svud->info.remote_ip = data_svud->rmt_ip;
    data_svud->rmt_ip = rmt_ip;

    data_svud->user_srvc = l_service_frid(S);
    data_svud->outconn = true;
    ntf.sock_srvc = l_service_svid(S);
    l_send_message(E, data_svud->user_srvc, L_MSG_SOCK_READY_NTF, &ntf, sizeof(l_sock_ready_ntf));
    }
    break;
  /* common socket messages from master */
  case L_MSG_SOCK_READY_RX:
    if ((msg = (l_message*)l_squeue_top(&svud->rdmq))) {
      data = msg->mssg_data + msg->mgid_cust;
      size = msg->data_size - msg->mgid_cust;
      done = l_data_read(S->ioev_hdl, data, size);
      if (done == size) {
        l_squeue_pop(&svud->wrmq);
        /* TODO: send L_MSG_SOCK_DATA_RX_RSP to user service */
      } else {
        msg->mgid_cust += done;
      }
    }
    break;
  case L_MSG_SOCK_READY_TX:
    if ((msg = (l_message*)l_squeue_top(&svud->wrmq))) {
      data = msg->mssg_data + msg->mgid_cust;
      size = msg->data_size - msg->mgid_cust;
      done = l_data_write(S->ioev_hdl, data, size);
      if (done == size) {
        l_squeue_pop(&svud->wrmq);
        l_message_reset(E, msg, svud->user_svid, L_MSG_SOCK_DATA_TX_RSP, 0, 0, 0);
        msg->extra.a = ++svud->wrid;
        l_send_message_impl(E, msg);
      } else {
        msg->mgid_cust += done;
      }
    }
    break;
  case L_MSG_SOCK_DISC_NTF:
  case L_MSG_SOCK_ERROR_NTF:
    l_send_message(E, svud->user_svid, L_MSG_SOCK_ERROR_NTF, 0, 0);
    break;
  /* messages from user service */
  case L_MSG_SOCK_DATA_RX_REQ:
    if (msg->data_size && msg->mssg_data) {
      msg->mssg_flags |= L_MSSG_FLAG_DONT_FREE_MSSG;
      msg->mgid_cust = 0; /* use mgid_cust to record how many data alrady read */
      l_squeue_push(&svud->rdmq, &msg->node);
    }
    break;
  case L_MSG_SOCK_DATA_TX_REQ:
    if (msg->data_size && msg->mssg_data) {
      msg->mssg_flags |= L_MSSG_FLAG_DONT_FREE_MSSG;
      msg->mgid_cust = 0; /* use mgid_cust to record how many data already written */
      l_squeue_push(&svud->wrmq, &msg->node);
    }
    break;
  case L_MSG_SOCK_RECOVER_REQ:
    break;
  case L_MSG_SOCK_CLOSE_CMD: {
    l_ulong from_id = l_service_frid(S);
    if (from_id == 0) {
      l_stop_service(E); /* stop and destroy self, socket will auto closed when destroy */
    } else {
      l_sock_disc_cmd cmd;
      cmd.data_svud = svud; /* this service is destroyed after this cmd */
      l_send_message(E, l_service_frid(S), L_MSG_SOCK_DISC_CMD, &cmd, sizeof(l_sock_disc_cmd));
    }
    break;
  case L_MSG_STOP_LISTEN_SREVER:
    if (l_service_frid(S)) {
    }
    break;
  default:
    break;
  }
}

