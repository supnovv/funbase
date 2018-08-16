
static void*
l_httpd_on_create(lnlylib_env* E)
{
}

static void
l_httpd_on_destroy(lnlylib_env* E)
{
}

static void
l_httpd_proc(lnlylib_env* E)
{
}

static l_service_define
l_httpd_service = {
  "lonely-httpd",
  l_httpd_on_create,
  l_httpd_on_destroy,
  l_httpd_proc
};

