#define LNLYLIB_API_IMPL
#include "net/sock.h"

/** socket listen service **/

typedef struct {
  l_sockaddr local;
  l_service_callback* inconn_cb;
  l_umedit conns;
  l_dqueue connq;
  l_dqueue freeq;
} l_socket_listen_svud;

typedef struct {
  l_uint s[128/sizeof(l_uint)];
} l_ipaddr;

typedef struct {
  l_linknode node;
  l_socket_listen_svud* listen_svud;
  l_ulong listen_svid;
  l_ulong upper_svid;
  l_sbuf64 rmt_ip;
  l_ushort rmt_port;
  l_squeue wrmq;
  l_squeue rdmq;
  l_umedit wrid;
  l_umedit rdid;
} l_socket_inconn_svud;

static void* l_socket_inconn_service_on_create(lnlylib_env* E);
static void l_socket_inconn_service_on_destroy(lnlylib_env* E);
static void l_socket_inconn_service_proc(lnlylib_env* E);

static l_service_callback
l_socket_inconn_service_cb = {
  l_socket_inconn_service_on_create,
  l_socket_inconn_service_on_destroy,
  l_socket_inconn_service_proc
};

static void*
l_socket_listen_service_on_create(lnlylib_env* E)
{
  l_socket_listen_svud* data = 0;
  data = L_MALLOC_TYPE(E, l_socket_listen_svud);
  data->conns = 0;
  l_dqueue_init(&data->connq);
  l_dqueue_init(&data->freeq);
  return data;
}

static void
l_socket_listen_service_on_destroy(lnlylib_env* E)
{
  l_socket_listen_svud* data = 0;

  data = (l_socket_listen_svud*)E->svud;
  if (data) {
    L_MFREE(E, data);
  }

  E->svud = 0;
}

static void
l_socket_listen_service_accept_conn(void* ud, l_socketconn* conn)
{
  lnlylib_env* E = (lnlylib_env*)ud;
  l_socket_listen_svud* data = 0;
  l_socket_inconn_svud* inconn_svud = 0;
  l_sockaddr* remote = 0;

  data = (l_socket_listen_svud*)E->svud;
  inconn_svud = (l_socket_inconn_svud*)l_dqueue_pop(&data->freeq);
  if (inconn_svud == 0) {
    inconn_svud = L_MALLOC_TYPE(E, l_socket_inconn_svud);
  }

  remote = &conn->remote;
  inconn_svud->listen_svud = data;
  inconn_svud->listen_svid = E->S->srvc_id;
  inconn_svud->upper_svid = 0;
  inconn_svud->rmt_port = l_sockaddr_port(remote);
  inconn_svud->rmt_ip = l_sockaddr_getip(remote);

  l_create_service(E, L_USEHDL_SERVICE(conn->sock, &l_socket_inconn_service_cb), inconn_svud);
}

static void
l_socket_listen_service_proc(lnlylib_env* E)
{
  l_umedit mgid = 0;
  l_service* S = E->S;
  l_socket_listen_svud* srvc = 0;

  mgid = E->MSG->mssg_id;
  srvc = (l_socket_listen_svud*)E->svud;

  switch (mgid) {
  case L_MSG_SERVICE_EXIT_REQ:
    break;
  case L_MSG_SERVICE_RESTART:
    break;
  /* messages from master */
  case L_MSG_SOCK_ACCEPT_IND:
    l_socket_accept(S->ioev_hdl, l_socket_listen_service_accept_conn, E);
    break;
  case L_MSG_SOCK_ERROR:
    break;
  case L_MSG_SUBSRVC_CREATE_RSP: {
      l_subsrvc_create_rsp* rsp = 0;
      rsp = (l_subsrvc_create_rsp*)E->MSG->mssg_data;
      if (rsp->succ) {
        l_send_message(E, rsp->svid, L_MSG_SOCK_CONNECTED, 0, 0);
      } else {
        l_socket_inconn_svud* inconn_svud = 0;
        inconn_svud = (l_socket_inconn_svud*)rsp->svud;
        l_dqueue_push(&srvc->freeq, &inconn_svud->node);
      }
    }
    break;
  /* messages from accepted connection services */
  case L_MSG_SOCK_DISC_NTF:
    break;
  default:
    break;
  }
}

/** socket inconn service **/

static void*
l_socket_inconn_service_on_create(lnlylib_env* E)
{
  l_socket_inconn_svud* svud = 0;
  svud = (l_socket_inconn_svud*)E->svud;
  l_create_service(E, L_SERVICE(svud->listen_svud->inconn_cb), 0);
  return 0;
}

static void
l_socket_inconn_service_on_destroy(lnlylib_env* E)
{
  L_UNUSED(E);
}

static void
l_socket_inconn_service_proc(lnlylib_env* E)
{
  /** message exchanges, RSP only send after REQ, NTF can send without REQ **
  L_MSG_SOCK_CONNIND => 
  L_MSG_SOCK_CONNECT <=
                     => L_MSG_SOCK_CONNDONE  =>  L_MSG_SOCK_CONN_NTF
                        L_MSG_SOCK_DISC_REQ  <=
                                             =>  L_MSG_SOCK_DISC_NTF (after disc, the data service is destroyed)
                     => L_MSG_SOCK_DISCDONE  =>  L_MSG_SOCK_DISC_NTF
                     => L_MSG_SOCK_ERROR     =>  L_MSG_SOCK_DISC_NTF
                     => L_MSG_DATA_READY_RX  =>  L_MSG_READ_DATA_IND
                        L_MSG_READ_DATA_REQ  <=
                                             =>  L_MSG_READ_DATA_RSP
                        L_MSG_WRITE_DATA_REQ <=
                     => L_MSG_DATA_READY_TX  =>  L_MSG_WRITE_DATA_RSP
  ********************************************************************/

  l_message* MSG = E->MSG;
  l_service* S = E->S;
  l_socket_inconn_svud* svud = 0;

  l_message* msg = 0;
  l_byte* data = 0;
  l_umedit size = 0;
  l_umedit done = 0;

  svud = (l_socket_inconn_svud*)E->svud;

  switch (MSG->mssg_id) {
  case L_MSG_SUBSRVC_CREATE_RSP: {
      l_subsrvc_create_rsp* rsp = 0;
      rsp = (l_subsrvc_create_rsp*)MSG->mssg_data;
      if (rsp->succ) {
        svud->upper_svid = rsp->svid;
      } else {
        /* TODO: disc the socket and stop the service */
      }
    }
    break;
  /* messages from lower socket events */
  case L_MSG_SOCK_CONNECTED: /* the link is connected, send L_MSG_SOCK_CONN_NTF to upper */
    break;
  case L_MSG_SOCK_DISCONNECTED: /* the link is disconnected, send L_MSG_SOCK_DISC_NTF to upper */
    break;
  case L_MSG_SOCK_ERROR: /* send L_MSG_SOCK_DISC_NTF to upper */
    break;
  case L_MSG_DATA_READY_RX: /* read data and may send L_MSG_READ_DATA_RSP to upper */
    if ((msg = (l_message*)l_squeue_top(&svud->rdmq))) {
      data = msg->mssg_data + msg->mgid_cust;
      size = msg->data_size - msg->mgid_cust;
      done = l_data_read(S->ioev_hdl, data, size);
      if (done == size) {
        l_squeue_pop(&svud->wrmq);
        /* TODO */
      } else {
        msg->mgid_cust += done;
      }
    }
    break;
  case L_MSG_DATA_READY_TX: /* write data and may send L_MSG_WRITE_DATA_RSP to upper */
    if ((msg = (l_message*)l_squeue_top(&svud->wrmq))) {
      data = msg->mssg_data + msg->mgid_cust;
      size = msg->data_size - msg->mgid_cust;
      done = l_data_write(S->ioev_hdl, data, size);
      if (done == size) {
        l_squeue_pop(&svud->wrmq);
        l_message_reset(E, msg, svud->upper_svid, L_MSG_WRITE_DATA_RSP, 0, 0, 0);
        msg->extra.a = ++svud->wrid;
        l_send_message_impl(E, msg);
      } else {
        msg->mgid_cust += done;
      }
    }
    break;
  /* messages from upper service */
  case L_MSG_READ_DATA_REQ: /* queue the read request */
    if (msg->data_size && msg->mssg_data) {
      msg->mssg_flags |= L_MSSG_FLAG_DONT_FREE_MSSG;
      msg->mgid_cust = 0; /* use mgid_cust to record how many data alrady read */
      l_squeue_push(&svud->rdmq, &msg->node);
    }
    break;
  case L_MSG_WRITE_DATA_REQ: /* queue the write request */
    if (msg->data_size && msg->mssg_data) {
      msg->mssg_flags |= L_MSSG_FLAG_DONT_FREE_MSSG;
      msg->mgid_cust = 0; /* use mgid_cust to record how many data already written */
      l_squeue_push(&svud->wrmq, &msg->node);
    }
    break;
  case L_MSG_SOCK_DISC_REQ: /* send L_MSG_SOCK_DISC_NTF to upper */
    break;
  default:
    break;
  }
}

/** socket outconn service **/

typedef struct {
  l_sockaddr local;
  l_sbuf64 rmt_ip;
  l_ushort rmt_port;
} l_socket_outconn_svud;

static void
l_socket_outconn_service_proc(lnlylib_env* E)
{
  l_umedit mgid = E->MSG->mssg_id;
  l_service* S = E->S;
  l_socket_outconn_svud* outconn = 0;

  outconn = (l_socket_outconn_svud*)E->svud;
  L_UNUSED(outconn);

  switch (mgid) {
  case L_MSG_SOCK_CONNECT_IND:
    if (l_socket_cmpl_connect(S->ioev_hdl)) {
      /* socket connected success */
      l_send_message(E, S->srvc_id, L_MSG_DATA_READY_TX, 0, 0);
    } else {
      l_send_message(E, S->srvc_id, L_MSG_SOCK_DISCONNECTED, 0, 0);
    }
    break;
  default:
    break;
  }
}

