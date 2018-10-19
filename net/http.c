#define LNLYLIB_API_IMPL
#include "net/http.h"

enum l_http_version_enum {
  L_HTTP_VER_0NN,
  L_HTTP_VER_10N,
  L_HTTP_VER_11N,
  L_HTTP_VER_2NN,
  L_HTTP_VERSION_SIZE
};

static const l_strn l_http_versions[] = {
  l_literal_strn("HTTP/0"),
  l_literal_strn("HTTP/1.0"),
  l_literal_strn("HTTP/1.1"),
  l_literal_strn("HTTP/2")
};

typedef struct {
  l_int code;
  l_strn phrase;
} l_http_status_t;

typedef enum {
  L_HTTP_100_CONTINUE,
  L_HTTP_101_SWITCHING_PROTOCOLS,
  L_HTTP_200_OK,
  L_HTTP_201_CREATED,
  L_HTTP_202_ACCEPTED,
  L_HTTP_203_NON_AUTHORITATIVE_INFORMATION,
  L_HTTP_204_NO_CONTENT,
  L_HTTP_205_RESET_CONTENT,
  L_HTTP_206_PARTIAL_CONTENT,
  L_HTTP_300_MULTIPLE_CHOICES,
  L_HTTP_301_MOVED_PERMANENTLY,
  L_HTTP_302_FOUND,
  L_HTTP_303_SEE_OTHER,
  L_HTTP_304_NOT_MODIFIED,
  L_HTTP_305_USE_PROXY,
  L_HTTP_307_TEMPORARY_REDIRECT,
  L_HTTP_400_BAD_REQUEST,
  L_HTTP_401_UNAUTHORIZED,
  L_HTTP_402_PAYMENT_REQUIRED,
  L_HTTP_403_FORBIDDEN,
  L_HTTP_404_NOT_FOUND,
  L_HTTP_405_METHOD_NOT_ALLOWED,
  L_HTTP_406_NOT_ACCEPTABLE,
  L_HTTP_407_PROXY_AUTHENTICATION_REQUIRED,
  L_HTTP_408_REQUEST_TIMEOUT,
  L_HTTP_409_CONFLICT,
  L_HTTP_410_GONE,
  L_HTTP_411_LENGTH_REQUIRED,
  L_HTTP_412_PRECONDITION_FAILED,
  L_HTTP_413_REQUEST_ENTITY_TOO_LARGE,
  L_HTTP_414_REQUEST_URI_TOO_LONG,
  L_HTTP_415_UNSUPPORTED_MEDIA_TYPE,
  L_HTTP_416_REQUESTED_RANGE_NOT_SATISFIABLE,
  L_HTTP_417_EXPECTATION_FAILED,
  L_HTTP_500_INTERNAL_SERVER_ERROR,
  L_HTTP_501_NOT_IMPLEMENTED,
  L_HTTP_502_BAD_GATEWAY,
  L_HTTP_503_SERVICE_UNAVAILABLE,
  L_HTTP_504_GATEWAY_TIMEOUT,
  L_HTTP_505_HTTP_VERSION_NOT_SUPPORTED,
  L_HTTP_BAD_STATUS_CODE,
  L_HTTP_STATUS_SIZE
} l_http_status_enum;

static const l_http_status_t l_http_status[] = { /* https://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html */
  {100, l_literal_strn("Continue")}, /* 1xx - informational code */
  {101, l_literal_strn("Switching Protocols")},
  {200, l_literal_strn("OK")}, /* 2xx - success code */
  {201, l_literal_strn("Created")},
  {202, l_literal_strn("Accepted")},
  {203, l_literal_strn("Non-Authoritative Information")},
  {204, l_literal_strn("No Content")},
  {205, l_literal_strn("Reset Content")},
  {206, l_literal_strn("Partial Content")},
  {300, l_literal_strn("Multiple Choices")}, /* 3xx - redirection status */
  {301, l_literal_strn("Moved Permanently")},
  {302, l_literal_strn("Found")},
  {303, l_literal_strn("See Other")},
  {304, l_literal_strn("Not Modified")},
  {305, l_literal_strn("Use Proxy")},
  {307, l_literal_strn("Temporary Redirect")},
  {400, l_literal_strn("Bad Request")}, /* 4xx - request error */
  {401, l_literal_strn("Unauthorized")},
  {402, l_literal_strn("Payment Required")},
  {403, l_literal_strn("Forbidden")},
  {404, l_literal_strn("Not Found")},
  {405, l_literal_strn("Method Not Allowed")},
  {406, l_literal_strn("Not Acceptable")},
  {407, l_literal_strn("Proxy Authentication Required")},
  {408, l_literal_strn("Request Timeout")},
  {409, l_literal_strn("Conflict")},
  {410, l_literal_strn("Gone")},
  {411, l_literal_strn("Length Required")},
  {412, l_literal_strn("Precondition Failed")},
  {413, l_literal_strn("Request Entity Too Large")},
  {414, l_literal_strn("Request-URI Too Long")},
  {415, l_literal_strn("Unsupported Media Type")},
  {416, l_literal_strn("Requested Range Not Satisfiable")},
  {417, l_literal_strn("Expectation Failed")},
  {500, l_literal_strn("Internal Server Error")}, /* 5xx - server error */
  {501, l_literal_strn("Not Implemented")},
  {502, l_literal_strn("Bad Gateway")},
  {503, l_literal_strn("Service Unavailable")},
  {504, l_literal_strn("Gateway Timeout")},
  {505, l_literal_strn("HTTP Version Not Supported")}
};

typedef struct {
  l_int size;
  l_http_status_t* status;
} l_status_group_t;

static l_status_group_t l_status_groups[5] = {
  {2, l_http_status + 0},
  {7, l_http_status + 2},
  {8, l_http_status + 9},
  {18, l_http_status + 17},
  {6, l_http_status + 35}
};

static l_http_status_enum
l_translate_http_status(l_int code)
{
  l_int group = code / 100;

  if (group >= 1 && group <= 5) {
    code -= group * 100;
    if (code < l_status_groups[group - 1].size) {
      return l_status_groups[group - 1].status - l_http_status;
    }
  }

  return L_HTTP_BAD_STATUS_CODE;
}

/** Hypertext Transfer Protocol (HTTP) **

(1) HTTP VERSION

. Wikipedia

The first definition of HTTP/1.1, the version of HTTP in common use,
occurred in RFC 2068 in 1997, although this was made obsolete by
RFC 2616 in 1999 and then again by the RFC 7230 family of RFCs in
2014. A later version, the successor HTTP/2, was standardized in 2015,
and is now supported by major web servers and browsers over TLS using
ALPN extension where TLS 1.2 or newer is required.

HTTP/1.1 is a revision of the original HTTP (HTTP/1.0). In HTTP/1.0 a
separate connection to the same server is made for every resource
request. HTTP/1.1 can reuse a connection multiple times to download
images, scripts, stylesheets, etc after the page has been delivered.
HTTP/1.1 communications therefore experience less latency as the
establishment of TCP connections presents considerable overhead.

The first documented version of HTTP was HTTP V0.9 (1991). Dave Raggett
led the HTTP Working Group (HTTP WG) in 1995 and wanted to expand the
protocol with extended operations, extended negotiation, richer meta-
information, tied with a security protocol which became more efficient
by adding additional methods and header fields. RFC 1945 officially
introduced and recognized HTTP V1.0 in 1996.

The HTTP WG planned to publish new standards in December 1995 and the
support for pre-standard HTTP/1.1 based on the then developing RFC 2068
(called HTTP-NG) was rapidly adopted by the major browser developers in
early 1996. The HTTP/1.1 standard as defined in RFC 2068 was officially
released in January 1997. Improvements and updates to the HTTP/1.1
standard were released under RFC 2616 in June 1999. In 2007, the HTTPbis
Working Group was formed, in part, to revise and clarify the HTTP/1.1
specification. In June 2014, the WG released an updated six-part
specification obsoleting RFC 2616:

RFC 7230, HTTP/1.1: Message Syntax and Routing
RFC 7231, HTTP/1.1: Semantics and Content
RFC 7232, HTTP/1.1: Conditional Requests
RFC 7233, HTTP/1.1: Range Requests
RFC 7234, HTTP/1.1: Caching
RFC 7235, HTTP/1.1: Authentication

HTTP/2 was published as RFC 7540 in May 2015.

| Year | HTTP Version | RFC specification
| ---- | ------------ | -----------------
| 1991 | HTTP/0.9     | N/A
| 1996 | HTTP/1.0     | RFC 1945 (obsolete)
| 1997 | HTTP/1.1     | RFC 2068 (obsolete)
| 1999 | HTTP/1.1     | RFC 2616 (obsolete)
| 2014 | HTTP/1.1     | RFC 7230 ~ RFC 7235
| 2015 | HTTP/2       | RFC 7540

Version 1.1 of the protocol also made bandwidth optimization improvements     |
to HTTP/1.0. For example, HTTP/1.1 introduced chunked transfer encoding       | chunked transfer encoding
to allow content on persistent connections to be streamed rather than         |
buffered. HTTP pipelining further reduces lag time, allowing clients to       | HTTP pipelining
send multiple requests before waiting for each response. Another addition     |
to the protocol was byte serving, where a server transmits just the           | byte serving
portion of a resource explicitly requested by a client.                       |

HTTP is a stateless protocl. A stateless protocol does not require the        | stateless protocol
HTTP server to retain information or status about each user for the           |
duration of multiple requests. However, some web applications implement       |
states or server side sessions using for instance HTTP cookies or hidden      | HTTP cookies
variables within web forms.                                                   | hidden variables within web forms

The most popular way of establishing an encrypted HTTP connection is          |
HTTPS. Two other methods for establishing an encrypted HTTP connection        |
also exist: Secure Hypertext Transfer Prtocol (S-HTTP), and using the         |
HTTP/1.1 Upgrade header to specify an upgrade to TLS. Browser support         |
for these two is, however, nearly non-existent.                               |

. HTTP/1.1 RFC 7230

HTTP has been in use since 1990. The first version, later referred to          | HTTP/0.9
as HTTP/0.9, was a simple protocol for hypertext data transfer across
the Internet, using only a single request method (GET) and no metadata.
Since HTTP/0.9 did not support header fields in a request, there is
no mechanism for it to support name-based virtual hosts (selection of
resource by inspection of the Host header field). Any server that
implements name-base virtual hosts ought to disable support for HTTP/0.9.
Most requests that appear to be HTTP/0.9 are, in fact, badly constructed
HTTP/1.x requests caused by a client failing to properly encode the
request-target (the request uri).

HTTP/1.0, as defined by RFC 1945, added a range of request methods and        | HTTP/1.0
MIME-like messaging, allowing for metadata to be transferred and
modifiers placed on the request/response semantics. However, HTTP/1.0
did not sufficiently take into consideration the effects of hierarchical
proxies, caching, the need for persistent connections, or name-based
virtual hosts. The proliferation of incompletely implemented applications
calling themselves "HTTP/1.0" further necessitated a protocol version
change in order for two communicating applications to determine ecah
other's true capabilities.

HTTP/1.1 remains compatible with HTTP/1.0 by including more stringent         | HTTP/1.1
requirements that enable reliable implementations, adding only those
features that can either be safely ignored by an HTTP/1.0 recipient
or only be sent when communicating with a party advertising
conformance with HTTP/1.1.

HTTP/1.1 has been designed to make supporting previous versions easy.
A general-purpose HTTP/1.1 server ought to be able to understand any
valid request in the format of HTTP/1.0, responding appropriately
with an HTTP/1.1 message that only uses features understood (or
safely ignored) by HTTP/1.0 clients. Likewise, an HTTP/1.1 client
can be expected to understand any valid HTTP/1.0 response.

This section summarizes major differences between versions HTTP/1.0
and HTTP/1.1.

The requirements that clients and servers support the Host header             | Multihomed Web Servers
field, report an error if it is missing from an HTTP/1.1 request, and
accept absolute URIs are among the most important changes defined by
HTTP/1.1.

Older HTTP/1.0 clients assumed a one-to-one relationship of IP
addresses and servers; there was no other established mechnism for
distinguishing the intended server of a request then the IP address
to which that request was directed. The Host header field was
introduced during the development of HTTP/1.1 and, though it was
quickly implemented by most HTTP/1.0 broswers, additional requirements
were placed on all HTTP/1.1 requests in order to ensure complete
adoption. At the time of this writing, most HTTP-based services are
dependent upon the Host header field for targeting requests.

(to be continue...)


(2) MESSAGE SYNTAX

. HTTP/1.1 RFC 7230 7231

This specification uses the Augmented Backus-Naur Form (ABNF) notation
of RFC5234 with a list extension, that allows for compact definition
of comma-separated lists using a '#' operator. As a convention, ABNF rule
names prefixed with "obs-" denote "obsolete" grammar rules that appear
for historical reasons.

The following core rules are included by reference, as defined in RFC5234.
ALPHA (letters), CR (carriage return), CRLF (CR LF), CTL (controls),
DIGIT (decimal 0-9), DQUOTE (double quote), HEXDIG (hexadecimal digit),
HTAB (horizontal tab), LF (line feed), OCTET (any 8-bit sequence of data),
SP (a single space), and VCHAR (any visible USASCII chracter).

ABNF notation:

  . literal strings (like "/"), upper case variables (like CRLF) are terminals
  . lower case variables in '<' and '>' are non-terminals (like <http-version>)
  . meta characters like * ( [ # have special meanings to building the notation structure
  . *( <a> ) - match zero or more <a>
  . +( <a> ) - match one or more <a>
  . [ <a> ] - match zero or one <a>, i.e., <a> is optional

HTTP relies upon the Uniform Resource Identifier (URI) standard RFC3986 to
indicate the target resource and relationships between resources.

Client request:

GET /hello.txt HTTP/1.1                                                     | <method> <request-url> <http-version> - request line
User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.71 zlib/1.2.3            | <request-header>
Host: www.example.com                                                       | ...
Accept-Language: en, mi                                                     |

Server response:

HTTP/1.1 200 OK                                                             | <http-version> <status-code> <phrase> - status line
Date: Mon, 27 Jul 2009 12:28:53 GMT                                         | <response-header>
Server: Apache                                                              | ...
Last-Modified: Wed, 22 Jul 2009 19:15:56 GMT                                |
ETag: "24aa387-d-1568eb00"                                                  |
Accept-Ranges: bytes                                                        |
Content-Length: 51                                                          |
Vary: Accept-Encoding                                                       |
Content-Type: text/plain                                                    |
                                                                            | <empty-line> - separate the header and the body
Hello World! My payload includes a trailing CRLF.                           | <body>

A server responds to a client's request by sending one or more HTTP
response messages. A connection might be used for multiple request/
response exchanges.

HTTP enables the use of intermediaries to satisfy requests through a          |
chain of connections. There are three common forms of HTTP                    |
intermediary: proxy, gateway, and tunnel. In some cases, a single             |
intermediary might act as an origin server, proxy, gateway, or                | origin server, proxy, gateway, tunnel
tunnel, switching behavior based on the nature of each request.               |

HTTP does not have specific length limitations for many of its
protocol elements because the lengths that might be appropriate will
vary widely, depending on the deployment context and purpose of the
implementation.

At a minimum, a recipient MUST be albe to parse and process protocol
element lengths that are at least as long as the velus that it
generates for those same protocol elements in other messages. For
example, an origin server that publishes very long URI references to
its own resources needs to be able to parse and process those same
references when received as a request target.

A recipient MUST interpret a received protocol element according to
the semantics defined for it by this specification, including
extensions to this specification, unless the recipient has determined
(through experience or configuration) that the sender incorrectly
implements what is implied by those semantics. For example, an
origin server might disregard the contents of a received
Accept-Encoding header field if inspection of the User-Agent header
field indicates a specific implementation version that is known to
fail on receipt of certain content codings.

Unless noted otherwise, a recipient MAY attempt to recover a usable
protocol element from an invalid construct. HTTP does not define
specific error handling manchanisms exept when they have a direct
impact on security, since different applications of the protocol
require different error handling strategies. For example, a Web
browser might wish to transparently recover from a response where
the Location header field doesn't parse according to the ABNF,
whereas a systems control client might consider any form of error
recovery to be dangerous.


<http-message> : <start-line>
	 *( <header-field> CRLF )
	 CRLF
	 [ <message-body> ]

<start-line>   : <request-line>
       | <status-line>


<http-version> : <http-name> "/" DIGIT "." DIGIT
<http-name>    : %x48.54.54.50 ; "HTTP", case-sensitive                       | the http name is case-sensitive

A sender MUST NOT send whitespace between the start-line and the first
header field. A recipient that receives whitespace between the
start-line and the first header field MUST either reject the message
as invalid or consume each whitespace-preceded line without further
processing of it (i.e., ignore the entire line, along with any
subsequent lines preceded by whitespace, until a properly formed
header field is received or the header section is terminated).

The presence of such whitespace in a request might be an attempt to
trick a server into ignoring that field or processing the line after
it as a new request, either of which might result in a security
vulnerability if other implementations within the request chain
interpret the same message differently. Likewise, the presence of
such whitespace in a response might be ignored by some clients or
cause others to cease parsing.


<request-line> : <http-method> SP <request-uri> SP <http-version> CRLF        | the request method is case-sensitive

Recipients typically parse the request-line into its component parts
by splitting on whitespace, since no whitespace is allowed in the
three components. Unfortunately, some user agents fail to properly
encode or exclude whitespace found in hypertext references, resulting
in those disallowed characters being sent in a request-uri.

Recipients of an invalid request-line SHOULD respond with either a 400
(Bad Request) error or a 301 (Moved Permanently) redirect with the
request-uri properly encoded. A recipient SHOULD NOT attempt to
autocorrect and the process the request without a redirect, since the
invalid request-line might be deliberately crafted to bypass security
filters along the request chain.


<status-line> : <http-version> SP <status-code> SP <reason-phrase> CRLF
<status-code> : 3DIGIT
<reason-phrase> : *( HTAB | SP | VCHAR | obs-text )

The reason phrase is a possibly empty textual phrase describing the
status code. The status code element is a 3-digit integer code
describing the result of the server's attempt to understand and satisfy
the client's corresponding request.

The reason-phrase element exists for the sole purpose of providing a
textual description associated with the numeric status code, mostly
out of deference to earlier Internet application protocols that were
more frequently used with interactive text clients. A client SHOULD
ignore the reason-phrase content.


<header-field> : <field-name> ":" OWS <field-value> OWS
<field-name>   : 'case-insensitive no whitespace separated name
<field-value>  : *( <field-content> | <obs-fold> )
<field-content>: <field-vchar> [ 1*( SP | HTAB ) <field-vchar> ]
<field-vchar>  : VCHAR | obs-text
<obs-fold>     : CRLF 1*( SP | HTAB )

No whitespace is allowed between the header field name and colon.
In the past, differences in the handling of such whitespace have
led to security vulnerabilities in request routing and response
handling. A server MUST reject any received request message that
contains whitespace between a header field name and colon with a
response code of 400 (Bad Request). A proxy MUST remove any such
whitespace from a response message before forwarding the message
downstream.

Most HTTP header field values are defined using common syntax
components (token, quoted-string, and comment) separated by
whitespace or specific delimiting characters. Delimiters are
chosen from the set of US-ASCII visual characters not allowed
in a token (DQUOTE and "(),/:;<=>?@[\]{}").

<token> : 1*tchar
<tchar> : "!" | "#" | "$" | "%" | "&" | "'" | "*"
| "+" | "-" | "." | "^" | "_" | "`" | "|"
| "~" | DIGIT | ALPHA                                                 | any VCHAR, except delimiters

<quoted-string> : DQUOTE *( <qdtext> | <quoted-pari> ) DQUOTE
<qdtext>        : HTAB | SP | %x21 | %x23-5B | %x5D-7E | <obs-text>
<obs-text>      : %x80-FF

<comment>       : "(" *( <ctext> | <quoted-pair> | <comment> ) ")"
<ctext>         : HTAB | SP | %x21-27 | %x2A-5B | %x5D-7E | <obs-text>
<quoted-pair>   : "\" ( HTAB | SP | VCHAR | <obs-text> )

A string of text is parsed as a single value if it is quoted using
double-quote marks. Comments can be included in some HTTP header
fields by surrounding the comment text with parentheses. Comments
are only allowed in fields containing "comment" as part of their
field value definition. The backslash octet ("\") can be used as
a single-octect quoting mechanism within quoted-stirng and comment
constructs. Recipients that process the value of a quoted-string
MUST handle a quoted-pair as if it were replaced by the octet
following the backslash. A sender SHOULD NOT generate a quoted pair
in a quoted-string except where necessary to quote DQUOTE and backslash
octets occurring within that string. A sender SHOULD NOT generate a
quote pair in a comment except where necessary to quote parentheses
"(" and ")" and backslash octets occuring within that comment.

Historically, HTTP header field values could be extended over
multiple lines by preceding each extra line with at least one
space or horizontal tab (obs-fold). This specification
deprecates such line folding except within the message/http
media type (Section 8.3.1). A sender MUST NOT generate a message
that includes line folding unless the message is intended for
packing within the message/http media type.

A server that receives an obs-fold in a request message that is
not within a message/http container MUST either reject the message
by sending a 400 (Bad Request), preferably with a representation
explaining that obsolete line folding is unacceptable, or replace
each received obs-fold with one or more SP octets prior to
interpreting the field value or forwarding the message downstream.

A proxy or gateway that receives an obs-fold in a response message
that is not within a message/http container MUST either discard the
message and replace it with a 502 (Bad Gateway) response, preferably
with a representation explaining that unacceptable line folding was
received, or replace each received obs-fold with one or more SP octets
prior to interpreting the field value or forwarding the message
downstream.

A user agent that receives an obs-fold in a response message that is
not within a message/http container MSUT replace each received obs-fold
with one or more SP octets prior to interpreting the field value.

Historically, HTTP has allowed field content with text in the ISO-8859-1
charset, supporting other charsets only through use of RFC2047 encoding.
In practice, most HTTP header field values use only a subset of the
US-ASCII. Newly defined header fields SHOULD limit their field values to
US-ASCII octets. A recipient SHOULD treat other octets in field content
(obs-text) as opaque data.

The order in which header fields with differing field names are
received is not significant. However, it is good practice to send
header fields that contain control data first, such as Host on
requests and Date on responses, so that implementations can decide
when not to handle a message as early as possible. A server MUST NOT
apply a request to the target resource until the entire request
header section is received, since later header fields might include
conditionals, authentication credentials, or deliberately misleading
duplicate header fields that would impact request processing.

A sender MUST NOT generate multiple header fields with the same name
in a message unless either the entire field value for that header is
defined as a comma-separated list (i.e., #(values)) or the header is
a well-known exception (as noted below).

A recipient MAY combine multiple header fields with the same name, by
appending each subsequent field value to the combined field value in
order, separated by a comma. The order in which header fields with the
same name are received is therefore significant to the interpretation
of the combined field value; a proxy MUST NOT change the order of these
field values when forwarding a message.

Note: In practice, the "Set-Cookie" header field (RFC6265) often
appears multiple times in a response message and does not use the
list syntax, violating the above requirements on multiple header
fields with the same name. Since it cannot be combined into a
single field name, recipients ought to handle "Set-Cookie" as a
special case while processing header fields.

This specification uses three rules to denote the use of linear
whitespace: OWS (optional whitespace), RWS (required whitespace), and
BWS ("bad" whitespace).

The OWS rule is used where zero or more linear whitespace octets
might apper. For protocol elements where OWS is preferred to improve
readability, a sender SHOULD generate the OWS as a single SP; otherwise,
a sender SHOULD NOT generate OWS except as needed to white out invalid
or unwanted protocol elements during in-place message filtering.

The RWS rule is used when at least one linear whitespace octet is
required to separate field tokens. A sender SHOULD generate RWS as
a single SP.

The BWS rule is used where the grammar allows OWS only for historical
reasons. A sender MUST NOT generate BWS in messages. A recipient MUST
parse for such BWS and remove it before interpreting the protocol
element.

HTTP does not place a predefined limit on the length of each header
field or on the length of the header section as a whole. Various ad
hoc limitations on individual header field length are found in practice,
often depending on the specific field semantics.

A server that receives a request header field, or set of fields, larger
than it wishes to process MUST respond with an appropriate 4xx (Client
Error) status code. Ignoring such header fields would increase the
server's vulnerability to request smuggling attacks.

A client MAY discard or truncate received header fields that are larger
than the client wishes to process if the field semantics are such that
the dropped value(s) can be safely ignored without changing the message
framing or response semantics.


(3) CONNECTION MANAGEMENT

. Wikipedia

In HTTP/0.9 and 1.0, the connection is closed after a single request/
response pair. In HTTP/1.1 a keep-alive-mechanism was introduced, where
a connection could be reused for more than one request. Such persistent
connections reduce request latency perceptibly, because the client does
not need to re-negotiate the TCP 3-Way-Handshake connection after the
first request has been sent. Another positive side effect is that in
general the connection becomes faster with time due to TCP's slow-start-
mechanism.

. HTTP/1.1 RFC 7230

The terms "upstream" and "downstream" are used to describe directional
requirements in relation to the message flow: all messages flow from
upstream to downstream. The terms "inbound" and "outbound" are used to
describe directional requirements in relation to the request route:
"inbound" means toward the origin server and "outbound" means toward
the user agent.

HTTP messaging is independent of the underlying transport- or session-
layer connection protocol(s). HTTP only presumes a reliable transport
with in-order delivery of requests and the corresponding in-order
delivery of responses.

HTTP implementations are expected to engage in connection management,         |
which includes maintaining the state of current connections,                  |
establishing a new connection or reusing an existing connection,              |
processing messages received on a connection, detecting connection            |
failures, and closing each connection. Most clients maintain multiple         |
connections in parallel, including more than one connection per               | client may maintain more than one connection
server endpoint. Most servers are designed to maintain thousands of           | per server endpoint.
concurrent connections, while controlling request queues to enable            |
fair use and detect denial-of-service attacks.                                | DOS attack

The "Connection" header field allows the sender to indicate desired
control options for the current connection. In order to avoid
confusing downstream recipients, a proxy or gateway MUST remove or
replace any received connection options before forwarding the message.

When a header field aside from Connection is used to supply control
information for or about the current connection, the sender MUST list
the corresponding field-name within the Connection header field. A
proxy or gateway MUST parse a received Connection header field before
a message is forward and, for each connection-option in this field,
remove any header field(s) from the message with the same name as the
connection-option, and then remove the Connection header field itself
(or replace it with the intermediary's own connection options for the
forwarded message).

Hence, the Connection header field provides a declarative way of              | Connection header provides a way to distinguishing
distinguish header fields that are only intended for the immediate            | which headers are only intended for the immediate
recipient ("hop-by-hop") from those fields that are intended for              | recipient from those are intended for all recipients.
all recipients on the chain ("end-to-end"), enabling the message              |
to be self-descriptive and allowing future connection-specific                |
extensions to be deployed without fear that they will be blindly              |
forwarded by older intermediaries.                                            |

<header-connection> : 1#<connection-option>                                   | one or more comma separated tokens
<connection-option> : <token>                                                 |

A sender MUST NOT send a connection option corresponding to a header
field that is intended for all recipients of the payload. For example,
Cache-Control is never appropriate as a connection option.

The connection options do not always correspond to a header field             | <connection-option>s do not always correspond to a header field,
present in the message, since a connection-specific header field              | since it might not be needed if there are no parameters.
might not be needed if there are no parameters associated with a              |
connection option. In contrast, a connection-specific header field            |
that is received without a corresponding connection option usually            | if a connection-specific header is received without a corresponding
indicates that the field has been improperly forwarded by an                  | connection option usually indicates improper forward.
intermediary and ought to be ignored by the recipient.                        |

When defining new connection options, specification authors ought to
survey existing header field names and ensure that the new connection
option does not share the same name as an already deployed header
field. The "close" connection option is defined for a sender to signal
that this connection will be closed after completion of the response.
For example,

  Connection: close

in either the request or the response header fields indicates that the
sender is going to close the connection after the current request/
response is complete.

A client that does not support persistent connections MUST send the
"close" connection option in every request message. A server that
does not support persistent connections MUST send the "close" connection
option in every response message that does not have a 1xx (Informational)
status code.

A client that sends a "close" connection option MUST NOT send further
requests on that connection and MUST close the connection after reading
the final response message corresponding to this request.

A server that receives a "close" connection option MUST initiate a
close of the connection after it sends the final response. The server
SHOULD send a "close" connection option in its final response on that
connection. The server MUST NOT process any further requests received on
that connection.

A server that sends a "close" connection option MUST initiate a close
of the connection after it sends the response containing "close". The
server MUST NOT process any further requests received on that connection.

A client that receives a "close" connection option MUST cease sending
requests on that connection and close the connection after reading the
response message containing the "close"; if additional pipelined requests
had been sent on the connection, the client SHOULD NOT assume that
they will be processed by the server.

If a server performs an immediate close of a TCP connection, there is a       | 服务端如果立即断连，客户端有收不到最后一个响应的风险
significant risk that the client will not be able to read the last HTTP       |
response. If the server receives additional data from the client on a         |
fully closed connection, such as another request that was sent by the         | 原因在于，如果服务端在关闭的连接上继续收到数据，TCP 协议栈
client before receiving the server's response, the server's TCP stack         | 将发送 reset 给对方，这可能清空客户端未确认的输入缓存数据
will send a reset packet to the client; unfortunately, the reset packet       |
might erase the client's unacknowledged input buffers before they can         |
be read and interpreted by the client's HTTP parser.                          |

To avoid the TCP reset problem, servers typically close a connection in       | 为避免 TCP reset problem，服务端首先只关闭 write 单边连接，
steps. First, the server performs a half-close by closing only the write      | 继续维护 read 操作，直到客户端关闭连接，或服务端等待了适当
side of read/write connection. The server then continues to read from         | 时间大概率确信已经收到客户端对响应包的确认（ACK）
the connection until it receives a corresponding close by the client, or      |
until the server is reasonably certain that its own TCP stack has             |
received the client's acknowledgement of the packet(s) containing the         |
server's last response. Finally, the server fully closes the connection.      |

It is unknown whether the reset problem is exclusive to TCP or might
also be found in other transport connection protocols.

HTTP/1.1 defaults to the use of "persistent connections", allowing            | persistent connections
multiple requests and responses to be carried over a single connection.       |
The "close" option is used to signal that a connection will not persist       |
after the current request/response. HTTP implementations SHOULD support       |
persistent connections.                                                       |

A recipient determines whether a connection is persistent or not based
on the most recently received message's protocol version and
Connection header field (if any):

  o If the "close" option is present, the connection will not persist
    after the current response; else,
  o If the received protocol is HTTP/1.1 (or later), the connection
    will persist after the current response; else,
  o If the received protocol is HTTP/1.0, the "keep-alive" connection
    option is present, the recipient is not a proxy, and the recipient
    wishes to honor the HTTP/1.0 "keep-alive" mechanism, the connection
    will persist after the current response; otherwise,
  o The connection will close after the current response.

A client MAY send additional requests on a persistent connection until
it sends or receives a "close" connection option or receives an HTTP/1.0
response without a "keep-alive" connection option.

In order to remain persistent, all messages on a connection need to have      | 为保持持久连接，消息必须自己拥有长度信息，
a self-defined message length (i.e., one not defined by closure of the        | 而不是通过断连结束消息（HTTP/0.9的做法）
connection). A server MUST read the entire request message body or close      |
the connection after sending its response, since otherwise the remaining      | 服务端发送响应后，必须读完前次请求所有数据，
data on a persistent connection would be misinterpreted as the next           | 或关闭连接
request. Likewise, a client MUST read the entire response messge body if      |
it intends to reuse the same connection for a subsequent request.             | 客户端必须读完前次响应数据，如果想重用连接发送下次请求

A proxy servber MUST NOT maintain a persistent connection with an HTTP/1.0
client. See Appendix A.1.2 for more information on backwards compatibility
whith HTTP/1.0 clients.

Connections can be closed at any time, with or without intention.             | retrying requests
Implementations ought to anticipate the need to recover from
asynchronous close events.

When an inbound connection is closed permaturely, a client MAY open a
new connection and automatically retransmit an aborted sequence of requests
if all of those requests have idempotent methods (rfc7231 4.2.2). A proxy
MUST NOT automatically retry non-idempotent requests.

A user agent MUST NOT automatically retry a request with a non-idempotent
method unless it has some means to know that the request semantics are
actually idempotent, regardless of the method, or some means to detect that
the original request was never applied. For example, a user agent that knows
(through design or configuration) that a POST request to a given resource is
safe can repeat that request automatically. Likewise, a user agent designed
specifically to operate on a version control repository might be able to
recover from partial failure conditions by checking the target resource
revision(s) after a failed connection, reverting or fixing any changes that
were partially applied, and then automatically retrying the requests that
failed.

A client SHOULD NOT automatically retry a failed automatic retry.

A client that supports persistent connections MAY "pipeline" its requests      | client MAY pipeline its requests
(i.e., send multiple requests without waiting for each response). A server
MAY process a sequence of pipelined requests in parallel if they all have
safe methods, but it MUST send the corresponding responses in the same
order that the requests were received.

A client that pipelines requests SHOULD retry unanswered requests if
the connection closes before it receives all of the corresponding responses.
When retrying pipelined requests after a failed connection (a connection
not explicitly closed by the server in its last complete response), a client
MUST NOT pipeline immediately after connection establishment, since the
first remaining request in the prior pipeline might have caused an error
response that can be lost again if multiple requests are sent on a
permaturely closed connection (see the TCP reset problem).

Idempotent methods are significant to pipelining because they can be
automatically retried after a connection failure. A user agent SHOULD NOT
pipeline requests after a non-idempotent method. until the final response
status code for that method has been received, unless the user agent has
a means to detect and recover from partial failure conditions involving
the pipelined sequence.

An intermediary that receives pipelined requests MAY pipeline those
requests when forwarding them inbound, since it can rely on the outbound
user agent(s) to determine what requests can be safely pipelined. If the
inbound connection fails before receiving a response, the pipelining
intermediary MAY attempt to retry a sequence of requests that have yet
to receive a response if the requests all have idempotent methods;
otherwise, the pipeling intermediary SHOULD forward any received
responses and then close the corresponding outbound connection(s) so
that the outbound user agent(s) can recover accordingly.

A client ought to limit the number of simultaneous open connections that
it maintains to a given server. Previous revisions of HTTP gave a
specific number of connections as a ceiling, but this was found to be
impractical for many applications. As a result, this specification does
not mandate a particular maximum number of connection but, instead,
encourages clients to be conservation when opening multiple connections.

Multiple connections are typically used to avoid the "head-of-line            | "head-of-line blocking" problem
blocking" problem, wherein a request that takes significant server-side       |
processing and/or has a large payload blocks subsequent requests on           |
the same connection. However, each connection consumes server resources.      |
Furthermore, using multiple connections can cause undesireable side           | multiple connections may cause undesireable
effects in congested networks.                                                | side effects in cogested networks.

Note that a server might reject traffic that it deems abusive or              | 服务端可以拒绝有 DOS 攻击嫌疑的请求，
characteristic of a denial-of-service attack, such as an excessive            | 比如一个客户端同时打开过多的连接
number of open connections from a single client.                              |

Servers will usually have some timeout value beyond which they will           | 服务端通常设置连接超时，断连不再活动的连接
no longer maintain an inactive connection. Proxy servers might make           |
this a higher value since it is likely that the client will be making         |
more connections through the same proxy server. The use of persistent         |
connections places no requirements on the length (or existence) of this       | 持久连接对超时时间值，没有规定
timeout for either the client or the server.                                  |

A client or server that wishes to time out SHOULD issue a graceful close
on the connection. Implementations SHOULD constantly monitor open
connections for a received closure signal and respond to it as
appropriate, since prompt closure of both sides of a connection enables
allocated system resources to be reclaimed.

A client, server, or proxy MAY close the transport connection at any
time. For example, a client might have started to send a new request
at the same time that the server has decided to close the "idle"
connection. From the server's point of view, the connection is being
closed while it was idle, but from the client's point of view, a
request is in progress.

A server SHOULD sustain persistent connections, when possible, and             | 但服务端应该尽可能的维持持久连接
allow the underlying transport's flow-control mechanisms to resolve            |
temporary overloads, rather than terminate connections with the                |
expectation that clients will retry. The latter technique can                  |
exacerbate network congestion.                                                 |

A client sending a message body SHOULD monitor the network connection
for an error response while it is transmitting the request. If the
client sees a response that indicates the server does not wish to
receive the message body and is closing the connection, the client
SHOULD immediately cease transmitting the body and close its side
of the connections.

The "Upgrade" header field is intended to provide a simple mechanism          | Upgrade header field
for transitioning from HTTP/1.1 to some other protocol on the same
connection. A client MAY send a list of protocols in the Upgrade header
field of a request to invite the server to switch to one or more of
those protocols, in order of descending preference, before sending
the final response. A server MAY ignore a received Upgrade header
field if it wishes to continue using the current protocol on that
connection, Upgrade cannot be used to insist on a protocol change.

<header-upgrade> : 1#<protocol>
<protocol>       : <protocol-name> ["/" <protocol-version>]
<protocol-name>  : <token>
<protocol-version> : <token>

A server that sends a 101 (Switching Protocols) response MUST send
an Upgrade header field to indicate the new protocol(s) to which
the connection is being swithed; if multiple protocol layers are
being switched, the sender MUST list the protocols in layer-ascending
order. A server MUST NOT switch to a protocol that was not indicated
by the client in the corresponding request's Upgrade header field. A
server MAY choose to ignore the order of preference indicated by the
client and select the new protocol(s) based on other factors, such as
the nature of the request or the current load on the serer.

A server that sends a 426 (Upgrade Required) response MUST send an
Upgrade header field to indicate the acceptable protocols, in order
of descending preference.

A server MAY send an Upgrade header field in any order response to
advertise that it implements support for upgrading to the listed
protocols, in order of descending preference, when appropriate for a
future request. (to be continue...)

(4) SECURITY CONSIDERATIONS

(5) ENCRYPTED HTTP

(6) AUTHENTICATION

*/

#define L_HTTP_MALLOC_TYPE(E, type) L_MALLOC_TYPE(E, type)
#define L_HTTP_MFREE(E, p) l_mfree(E, p)

#define L_HTTP_R_MAX_TX_SIZE (1023)
#define L_HTTP_R_MAX_RX_SIZE (8191)

typedef struct
  l_byte* parsing_start;
  l_byte* buffer_end;
  l_byte* current;
} l_input_buffer;

typedef struct {
  l_socket_data_service* data_service;
  l_ulong sock_svid;
  l_input_buffer in;
} l_socket_data_service_access_point;

typedef struct {
  l_socket_data_service_access_point sap;
  l_byte tx[L_HTTP_R_MAX_TX_SIZE + 1];
  l_byte rx[L_HTTP_R_MAX_RX_SIZE + 1];
  l_uint read_phase;
  l_int request_method;
  l_int http_version;
  l_byte* uri_start;
  l_byte* uri_end;
} l_http_accept_service;

typedef struct {
  l_service_access_point sap;
  l_socket_data_service* data_service;
} l_http_sender_service;

static l_bool l_http_accept_service_on_create(lnlylib_env* E);
static void l_http_accept_service_on_destroy(lnlylib_env* E);
static void l_http_accept_service_proc(lnlylib_env* E);

static l_service_callback l_http_accept_service_callback = {
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

static void l_http_accept_service_access_proc(lnlylib_env* E);

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

/** HTTP Message Syntax **

(1) Augmented BNF for Syntax Specification: ABNF (2008)

Internet technical specifications often need to define a formal
syntax. Over the years, a modified version of Backus-Naur Form
(BNF), called Augumented BNF (ABNF), has been popular among many
Internet specifications. The current specification decuments ABNF.
It balances compactness and simplicity with reasonable
representational power. The differences between standard BNF and
ABNF involve naming rules, repetition, alternatives, order-
independence, and value ranges.

. Rule Definition

Rule name is simply a sequence of characters, beginning with an
alphabetic character, and followed by a combination of alphabetics,
digits, and hyphens. Rule names are case insensitive.                          | 规则名称不区分大小写

Unlike original BNF, angle brackets ("<", ">") are not required.
However, angle brackets may be used around a rule name whenever their
presence facilitates in discerning the use of a rule name.

A rule is defined by the following sequence:

    name = elements crlf

where <name> is the name of the rule, <elements> is one or more rule
names or terminal specifications, and <crlf> is the end-of-line
indicator (carriage return followed by line feed). The equal sign
separates the name from the definition of the rule. The elements
forms a sequence of one or more rule names and/or value definitions,
combined according to the various operators defined in this document,
such as alternative and repetition.


(2) HTTP Message ABNF Formal Syntax

SP = %x20                                                                      | L_SP \x20, a single space character
CRLF = %x0D.0A                                                                 | L_CR \x0D \r L_LF \x0A \n, operator "." concat characters to character sequence (i.e., string)
ALPHA = %x41-5A / %x61-7A                                                      | alphabetic characters (i.e., letters)
DIGIT = %x30-39                                                                | decimal digit 0-9
HEXDIG = %x30-39 / "A" / "B" / "C" / "D" / "E" / "F"                           | hexdecimal digit, ABNF strings are case insensitive, i.e., "A" = %x41 / %x61
DQUOTE = %x22                                                                  |

GET = %x47.45.54
HEAD = %x48.45.41.44
OPTIONS = %x4F.50.54.4F.4E.53
POST = %x50.4F.53.54

request-message = request-line                                                 | 在 HTTP/1.0 之前，并不要求在请求行中包含 HTTP 版本信息
                  *( header-field CRLF )                                       | 实际是 HTTP/1.0 之前，整个请求报文只包含方法和请求URI，没有头部也没有消息体
                  CRLF                                                         | the empty line, used to separate the header and the body
                  [ message-body ]                                             |

request-line = http-method SP request-uri SP http-version CRLF                 | http-version may be not exist in HTTP/0.9
http-method = GET / HEAD / OPTIONS / POST / OTHER-METHOD                       | http-method is case-sensitive
http-version = http-name "/" DIGIT "." DIGIT                                   |
http-name = %x48.54.54.50                                                      | HTTP, i.e., http-name is case-sensitive

percent-encode = "%" HEXDIG HEXDIG                                             | encode delimiters or chars outside the allowed set, use %25 for "%", use upper case for consistency
reserved = generic-delim / subcomponent-delim                                  | the reserved character is NOT equivalent to its corresponding percent encode
generic-delim = ":" / "/" / "?" / "#" / "[" / "]" / "@"                        |
subcomponent-delim = "!" / $ / & / ' / ( / ) / * / + / , / ; / "="             |
unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"                             | the unreserved character is equivalent to its corresponding percent encode

authority = [ userinfo "@" ] host [ ":" port]
userinfo = *( unreserved / percent-encode / subcomponent-delim / ":" )         | Use of the format "user:password" in the userinfo is deprecated. Applications should
port = *DIGIT                                                                    not render any data after the first colon (":") found within a userinfo. The passing
host = ip-literal / ipv4-address / reg-name                                      of authentication information in clear text has proven to be a security risk in almost
ip-literal = "[" ( ipv6-address / ipvfuture ) "]"                                every case where it has been used.
ipvfuture = "v" 1*HEXDIG "." 1*( unreserved / subcomponent-delim / ":" )
reg-name = *( unreserved / percent-encode / subcomponent-delim )               | A registered name intended for lookup in the DNS
HEX16 = 1*4HEXDIG                                                                consists of a sequence of domain labels separated
LOW32 = ( HEX16 ":" HEX16 ) / ipv4-address                                       by ".". The label must start with a letter, end
ipv6-address =                                6(HEX16 ":") LOW32                 with a letter or digit, and have as interior
             /                           "::" 5(HEX16 ":") LOW32                 characters only letters, digits, and hyphen. There
             /                 [ HEX16 ] "::" 4(HEX16 ":") LOW32                 are some restrictions on the length. Labels must be
             / [ *1( HEX16 ":" ) HEX16 ] "::" 3(HEX16 ":") LOW32                 63 characters or less. To simplify implementations,
             / [ *2( HEX16 ":" ) HEX16 ] "::" 2(HEX16 ":") LOW32                 the total number of octets that represent a domain
             / [ *3( HEX16 ":" ) HEX16 ] "::" 1(HEX16 ":") LOW32                 name is limited to 255. (rfc 1034)
             / [ *4( HEX16 ":" ) HEX16 ] "::"              LOW32                 The restriction of host name on the first character
             / [ *5( HEX16 ":" ) HEX16 ] "::"              HEX16                 is relaxed to allow either a letter or a digit. Host
             / [ *6( HEX16 ":" ) HEX16 ] "::"                                    software MUST support this more literal syntax. Host
ipv4-address = DEC08 "." DEC08 "." DEC08 "." DEC08                               software MUST handle host names of up to 63
DEC08 = DIGIT / %x31-39 DIGIT / "1" 2DIGIT / "2" %x30-34 DIGIT / "25" %x30-35    characters and SHOULD handle host names of up to 255
                                                                                 charcters. (rfc 1123)
                                                                                 The reg-name allows percent-encode octets in order
                                                                                 to represent non-ASCII registered names in a uniform
                                                                                 way that is independent of the underlying name
                                                                                 resolution technology. Non-ASCII characters must first
                                                                                 be encoded according to UTF-8, and then each octet of
                                                                                 the UTF-8 sequence must be precent-encoded. URI producing
                                                                                 applications must not use percent-encoding in host unless
                                                                                 it is used to represent a UTF-8 character sequence.
                                                                                 When a non-ASCII registered name represents an internationalized
                                                                                 domain name intended for resolution via the DNS, the name
                                                                                 must be transformed to the IDNA encoding (rfc3490) prior
                                                                                 to name lookup. URI producers should provide these registered
                                                                                 names in the IDNA encoding, rather than a percent-encoding,
                                                                                 if they wish to maximize interoperability with legacy
                                                                                 URI resolvers.

path-abempty = *( "/" segment )
segment = *path-char
path-char = unreserved / percent-encode / subcomponent-delim / ":" / "@"
query = *( path-char / "/" / "?" )
fragment = *( path-char / "/" / "?" )

request-target = origin-form / absolute-form / authority-form / asterisk-form
origin-form = 1*("/" segment) [ "?" query ]
absolute-form = ("HTTP" / "HTTPS") ":" "//" authority path-abempty ["?" query]
authority-form = authority
asterisk-form = "*"

http-uri = "http:" "//" authority path-abempty [ "?" query ] [ "#" fragment ]   | The scheme and host are case-insensitive and normally provided in lowercase;
https-uri = "https:" "//" authority path-abempty [ "?" query ] [ "#" fragment ] | all other components are compared in a sensitive manner.


sresponse-message> : <status-line>
                     *( <header-field> CRLF )
                     CRLF
                     [ <message-body> ]

<status-line>  :
<status-line> : <http-version> SP <status-code> SP <reason-phrase> CRLF
<status-code> : 3DIGIT
<reason-phrase> : *( HTAB | SP | VCHAR | obs-text )
*/

/* request-line = request-method SP request-target SP http-version CRLF       | http-version may be not exist in HTTP/0.9
   request-method = GET / HEAD / OPTIONS / POST / OTHER-METHOD                | the request method is case-sensitive, for example GET = %x47.45.54
   http-version = HTTP "/" DIGIT "." DIGIT                                    | http-version is case-sensitive, i.e., HTTP = %x48.54.54.50
                                                                              |
   In theory, a client could receive requests and a server could receive      | 理论上，客户端可以接收请求，服务端可以接收响应。
   responses, distinguishing them by their different start-line formats,      | 但实践中，服务端一般实现为仅接收请求（响应被解析为非法请求），
   but, in practice, servers are implemented to only expect a request (a      | 而客户端仅接收响应。
   respons  is interpreted as an unknown or invalid request method) and       |
   clients are implemented to only expect a response.                         |
                                                                              |
   1. HTTP has been in use since 1990. The first version, later referred to   | HTTP/0.9 仅支持 GET 方法，请求行不包含 HTTP 版本信息，也没有头部。
   as HTTP/0.9, was a simple protocol for hypertext data transfer across      | 实际是 HTTP/1.0 之前，整个请求报文只有请求方法和 URI，没有头部和消息体。
   the Internet, using only a single request method (GET) and no metadata.    |
   Since HTTP/0.9 did not support header fields in a request, there is        |
   no mechanism for it to support name-based virtual hosts (selection of      |
   resource by inspection of the Host header field). Any server that          |
   implements name-base virtual hosts ought to disable support for HTTP/0.9.  |
   Most requests that appear to be HTTP/0.9 are, in fact, badly constructed   |
   HTTP/1.x requests caused by a client failing to properly encode the        |
   request-target (the request uri).                                          |
                                                                              |
   2. Recipients typically parse the request-line into its component parts    | 一般通过空白将请求行分成三部分，因为这三个组成部分都不包含空白。
   by splitting on whitespace (see Section 3.5), since no whitespace is       | 但客户端可能没正确编码，没正确处理 URI 中的空白字符，使空白出现在请求 URI 中。
   allowed in the three components.  Unfortunately, some user agents          |
   fail to properly encode or exclude whitespace found in hypertext           |
   references, resulting in those disallowed characters being sent in a       |
   request-target.                                                            |
                                                                              |
   3. Although the line terminator for the start-line and header fields is    | 尽管换行定义为 CRLF，但应识别单个 LF 作为换行（将前面的 CR 忽略，如果有）。
   the sequence CRLF, a recipient MAY recognize a single LF as a line         |
   terminator and ignore any preceding CR.                                    |
                                                                              |
   4. Although the request-line and status-line grammar rules require that    | 尽管开始行各部分定义为，由单个空白字符（SP）分隔，
   each of the component elements be separated by a single SP octet,          | 但应识别由不同空白字符形成的空白序列，这些空白字符
   recipients MAY instead parse on whitespace-delimited word boundaries       | 包括 SP，HTAB，VT (%x0B)，FF (%x0C)，或单独的 CR 。
   and, aside from the CRLF terminator, treat any form of whitespace as       | 而且应忽略开始行各组成部分的前置和后置空白。
   the SP separator while ignoring preceding or trailing whitespace;          |
   such whitespace includes one or more of the following octets: SP,          |
   HTAB, VT (%x0B), FF (%x0C), or bare CR.  However, lenient parsing can      | 然而宽松解析的另一面，可能导致安全问题。
   result in security vulnerabilities if there are multiple recipients        |
   of the message and each has its own unique interpretation of               |
   robustness (see Section 9.5).                                              |
                                                                              |
   5. Older HTTP/1.0 user agent implementations might send an extra CRLF      |
   after a POST request as a workaround for some early server                 |
   applications that failed to read message body content that was not         |
   terminated by a line-ending.  An HTTP/1.1 user agent MUST NOT preface      |
   or follow a request with an extra CRLF.  If terminating the request        |
   message body with a line-ending is desired, then the user agent MUST       |
   count the terminating CRLF octets as part of the message body length.      |
                                                                              |
   In the interest of robustness, a server that is expecting to receive       | 服务端应忽略请求行之前的空行（CRLF）。
   and parse a request-line SHOULD ignore at least one empty line (CRLF)      |
   received prior to the request-line.                                        |
                                                                              |
   6. Recipients of an invalid request-line SHOULD respond with either a      | 除以上几点，接收端不应自动纠正其他错误，应该回复
   400 (Bad Request) error or a 301 (Moved Permanently) redirect with         | 400 (Bad Request) 或 301 (Moved Permanently)。
   the request-target properly encoded.  A recipient SHOULD NOT attempt       |
   to autocorrect and then process the request without a redirect, since      |
   the invalid request-line might be deliberately crafted to bypass           |
   security filters along the request chain.                                  |
                                                                              |
   When a server listening only for HTTP request messages, or processing      |
   what appears from the start-line to be an HTTP request message,            |
   receives a sequence of octets that does not match the HTTP-message         |
   grammar aside from the robustness exceptions listed above, the server      |
   SHOULD respond with a 400 (Bad Request) response.                          |
                                                                              |
   7. HTTP does not have specific length limitations for many of its protocol | HTTP does not have specific length limitations for many elements.
   elements because the lengths that might be appropriate will vary widely,   |
   depending on the deployment context and purpose of the implementation.     |
   Hence, interoperability between senders and recipients depends on shared   |
   expectations regarding what is a reasonable length for each protocol       |
   element. Furthermore, what is commonly understood to be a reasonable length|
   for some protocol elements has changed over the course of the past two     |
   decades of HTTP use and is expected to continue changing in the future.    |
                                                                              |
   At a minimum, a recipient MUST be able to parse and process protocol       |
   element lengths that are at least as long as the values that it generates  |
   for those same protocol elements in other messages. For example, an origin |
   server that publishes very long URI references to its own rwsources needs  |
   to be able to parse and process those same references when received as a   |
   request target (request uri).                                              |
                                                                              |
   HTTP does not place a predefined limit on the length of a request-line.    |
   A server that receives a method longer than any that it implements         |
   SHOULD respond with a 501 (Not Implemented) status code. A server that     | 501 (Not Implemented)
   receives a request-target longer than any URI it wishes to parse MUST      |
   respond with a 414 (URI Too Long) status code. Various ad hoc limitations  | 414 (URI Too Long)
   on request-line length are found in practice. It is RECOMMENDED that all   |
   HTTP senders and recipients support, at a minimum, request-line lengths    | RECOMMENED support, at a minimum, request-line lengths of 8000 octets.
   of 8000 octets.                                                            |
*/

#define L_SOCK_READ_TIMEOUT (3000) /* 3s */

typedef enum {
  L_READ_REQUEST_METHOD,
  L_READ_REQUEST_URI,
  L_READ_HTTP_VERSION,
  L_READ_REQUEST_HEADERS,
  L_READ_REQUEST_BODY,
  L_READ_REQUEST_DONE,
  L_READ_PHASE_MAX_SIZE
} l_http_request_handle_phase;

typedef void (*l_http_receive_phase_func)(lnlylib_env* E, l_http_accept_service* accept_srvc);
static void l_http_read_request_method(lnlylib_env* E, l_http_accept_service* accept_srvc);
static void l_http_read_request_uri(lnlylib_env* E, l_http_accept_service* accept_srvc);
static void l_http_read_http_version(lnlylib_env* E, l_http_accept_service* accept_srvc);
static void l_http_read_request_headers(lnlylib_env* E, l_http_accept_service* accept_srvc);
static void l_http_read_request_body(lnlylib_env* E, l_http_accept_service* accept_srvc);

static l_http_receive_phase_func
l_http_receive_phases[L_READ_PHASE_MAX_SIZE] = {
  l_http_read_request_method,
  l_http_read_request_uri,
  l_http_read_http_version,
  l_http_read_request_headers,
  l_http_read_request_body
};

static void
l_http_receive_phase_handle(lnlylib_env* E, l_http_accept_service* accept_srvc)
{
  if (accept_srvc->read_phase < L_READ_REQUEST_DONE) {
    l_http_receive_phases[accept_srvc->read_phase](E, accept_srvc);
  }
}

static void
l_http_send_read_req(lnlylib_env* E, l_socket_data_service_access_point* sap)
{
  l_socket_data_read_req(E, sap->sock_svid, sap->in.current, sap->in.buffer_end, 1, L_SOCK_READ_TIMEOUT);
}

static void
l_http_accept_service_access_proc(lnlylib_env* E)
{
  l_http_accept_service* accept_srvc = (l_http_accept_service*)l_current_service_udata(E);
  l_message* msg = l_current_message(E);
  switch (l_message_id(msg)) {
  case L_MSG_SOCK_READY_NTF:
    l_http_send_read_req(E, &accept_srvc->sap);
    break;
  case L_MSG_SOCK_READ_RSP: {
    l_socket_read_rsp* rsp = (l_socket_read_rsp*)l_message_data(msg);
    if (rsp->bytes_read > 0) {
      accept_srvc->sap.in.current += rsp->bytes_read;
      l_http_receive_phase_handle(E, accept_srvc);
    } else {
      /* TODO - something error */
    }}
    break;
  case L_MSG_SOCK_WRITE_RSP:
    break;
  case L_MSG_SOCK_BAD_STATE_NTF:
    break;
  default:
    break;
  }
}

#define L_HTTP_METHOD_SUPPORT L_HTTP_M_HEAD

enum l_http_method_enum {
  L_HTTP_M_GET,
  L_HTTP_M_HEAD,
  L_HTTP_M_OPTIONS,
  L_HTTP_M_POST,
  L_HTTP_M_PUT,
  L_HTTP_M_DELETE,
  L_HTTP_M_CONNECT,
  L_HTTP_M_TRACE,
  L_HTTP_METHOD_SIZE
};

static const l_strn l_http_methods[] = {
  l_literal_strn("GET"),
  l_literal_strn("HEAD"),
  l_literal_strn("OPTIONS"),
  l_literal_strn("POST"),
  l_literal_strn("PUT"),
  l_literal_strn("DELETE"),
  l_literal_strn("CONNECT"),
  l_literal_strn("TRACE")
};

#define L_WHITESPACE_SIZE 6
#define L_NON_BREAK_WHITESPACE_SIZE 5
#define L_MAX_FIRST_LINE_PRECEDING_SPACES 80
#define L_MAX_SPACES_BETWEEN_FIELDS 80

static const l_strn
l_whitespace_list[6] = {
  l_literal_strn(L_S_SP),
  l_literal_strn(L_S_HT),
  l_literal_strn(L_S_VT),
  l_literal_strn(L_S_FF),
  l_literal_strn(L_S_CR),
  l_literal_strn(L_S_LF)
};

static l_string_pattern* l_whitespace_pattern;
static l_string_pattern* l_non_break_whitespace_pattern;
static l_string_pattern* l_request_method_pattern;

static void
l_http_prepare(lnlylib_env* E)
{
  l_whitespace_pattern = l_create_string_pattern(l_whitespace_list, L_WHITESPACE_SIZE);
  l_non_break_whitespace_pattern = l_create_string_pattern(l_whitespace_list, L_NON_BREAK_WHITESPACE_SIZE);
  l_request_method_pattern = l_create_string_pattern_case_sensitive(l_http_methods, L_HTTP_METHOD_SIZE); /* request method is case sensitive */
}

static void
l_http_cleanup(lnlylib_env* E)
{
  l_destroy_string_pattern(&l_whitespace_pattern);
  l_destroy_string_pattern(&l_non_break_whitespace_pattern);
  l_destroy_string_pattern(&l_request_method_pattern);
}

static void
l_http_read_request_method(lnlylib_env* E, l_http_accept_service* accept_srvc)
{
  l_input_buffer* in = &accept_srvc->sap.in;
  l_match_result match;

  /* ignore CRLF and preceding whitespace (notice 3. 4. 5.).
  so basically, need ignore any whitespaces preceding <request-method>.
  the whitespace need including at least SP, HTAB, VT, FF, CR, LF. */

  match = l_skip_space_and_match(l_whitespace_pattern, l_request_method_pattern, in->parsing_start, in->current);

  if (match.pend == l_string_too_short) {
    if (in->current - in->parsing_start > L_MAX_FIRST_LINE_PRECEDING_SPACES) {
      accept_srvc->read_phase = L_READ_REQUEST_DONE;
      l_http_write_response(E, accept_srvc, L_HTTP_400_BAD_REQUEST);
    } else {
      l_http_send_read_req(E, &accept_srvc->sap);
    }
  } else if (match.pend == 0) { /* match failed */
    accept_srvc->read_phase = L_READ_REQUEST_DONE;
    l_http_write_response(E, accept_srvc, L_HTTP_400_BAD_REQUEST);
  } else {
    if (match.string_i <= L_HTTP_METHOD_SUPPORT) {
      accept_srvc->request_method = match.string_i;
      accept_srvc->read_phase = L_READ_REQUEST_URI;
      in->parsing_start = match.pend;
      l_http_read_request_uri(E, accept_srvc);
    } else {
      accept_srvc->read_phase = L_READ_REQUEST_DONE;
      l_http_write_response(E, accept_srvc, L_HTTP_405_METHOD_NOT_ALLOWED);
    }
  }
}

/* request-uri = asterisk-form / origin-form / authority-form / absolute-form  |
   asterisk-form = "*"                                                         |
   origin-form = 1*("/" segment) [ "?" query ]                                 |
   authority-form = authority                                                  | The scheme name and host are case insensitive and normally provided in lowercase;
   absolute-form = ("http" / "https") "://" authority path-abempty ["?" query] | all other components are compared in a sensitive manner. A sender MUST NOT generate
                                                                               | http uri with an empty host identifer. A recipient that processes such a URI MUST
   http-scheme-uri = absolute-form ["#" fragment]                              | reject it as invalid.
                                                                               | The generic syntax for authority also includes a deprecated userinfo subcomponent.
   path-abempty = *( "/" segment )                                             | A sender MUST NOT generate the userinfo subcomponent and its "@" delimiter. Before
   segment = *path-char                                                        | making use of an http reference received from an untrusted source, a recipient
   path-char = unreserved / percent-encode / subcomponent-delim / ":" / "@"    | SHOULD parse for userinfo and treat its presence as an error; it is likely being
   query = *( path-char / "/" / "?" )                                          | used to obscure the authority for the sake of phishing attacks.
   fragment = *( path-char / "/" / "?" )                                       |
                                                                               |
   authority = [ userinfo "@" ] host [ ":" port]
   userinfo = *( unreserved / percent-encode / subcomponent-delim / ":" )      | Use of the format "user:password" in the userinfo is deprecated. Applications should
   port = *DIGIT                                                                 not render any data after the first colon (":") found within a userinfo. The passing
   host = ip-literal / ipv4-address / reg-name                                   of authentication information in clear text has proven to be a security risk in almost
   ip-literal = "[" ( ipv6-address / ipvfuture ) "]"                             every case where it has been used.
   ipvfuture = "v" 1*HEXDIG "." 1*( unreserved / subcomponent-delim / ":" )
   reg-name = *( unreserved / percent-encode / subcomponent-delim )            | A registered name intended for lookup in the DNS
   HEX16 = 1*4HEXDIG                                                             consists of a sequence of domain labels separated
   LOW32 = ( HEX16 ":" HEX16 ) / ipv4-address                                    by ".". The label must start with a letter, end
   ipv6-address =                                6(HEX16 ":") LOW32              with a letter or digit, and have as interior
                /                           "::" 5(HEX16 ":") LOW32              characters only letters, digits, and hyphen. There
                /                 [ HEX16 ] "::" 4(HEX16 ":") LOW32              are some restrictions on the length. Labels must be
                / [ *1( HEX16 ":" ) HEX16 ] "::" 3(HEX16 ":") LOW32              63 characters or less. To simplify implementations,
                / [ *2( HEX16 ":" ) HEX16 ] "::" 2(HEX16 ":") LOW32              the total number of octets that represent a domain
                / [ *3( HEX16 ":" ) HEX16 ] "::" 1(HEX16 ":") LOW32              name is limited to 255. (rfc 1034)
                / [ *4( HEX16 ":" ) HEX16 ] "::"              LOW32              The restriction of host name on the first character
                / [ *5( HEX16 ":" ) HEX16 ] "::"              HEX16              is relaxed to allow either a letter or a digit. Host
                / [ *6( HEX16 ":" ) HEX16 ] "::"                                 software MUST support this more literal syntax. Host
   ipv4-address = DEC08 "." DEC08 "." DEC08 "." DEC08                            software MUST handle host names of up to 63
   DEC08 = DIGIT / %x31-39 DIGIT / "1" 2DIGIT / "2" %x30-34 DIGIT / "25" %x30-35 characters and SHOULD handle host names of up to 255
                                                                                 charcters. (rfc 1123)
   percent-encode = "%" HEXDIG HEXDIG                                            The reg-name allows percent-encode octets in order
   unreserved-char = ALPHA / DIGIT / "-" / "." / "_" / "~"                       to represent non-ASCII registered names in a uniform
   reserved-char = generic-delim / subcomponent-delim                            way that is independent of the underlying name
   generic-delim = ":" / "/" / "?" / "#" / "[" / "]" / "@"                       resolution technology. Non-ASCII characters must first
   subcomponent-delim = "!" / $ / & / ' / ( / ) / * / + / , / ; / "="            be encoded according to UTF-8, and then each octet of
                                                                                 the UTF-8 sequence must be precent-encoded. URI producing
   A percent encoding mechanism is used to represent a data octect in a          applications must not use percent-encoding in host unless
   componet when that octect's corresponding charcter is outside the             it is used to represent a UTF-8 character sequence.
   allowed set or is being used as a delimiter of, or within, the                When a non-ASCII registered name represents an internationalized
   component. The uppercase hexadecimal digits 'A' through 'F' are               domain name intended for resolution via the DNS, the name
   equivalent to the lowercase digits 'a' through 'f', respectively.             must be transformed to the IDNA encoding (rfc3490) prior
   If two URIs differ only in the case of hexadecimal digits used in             to name lookup. URI producers should provide these registered
   percent encoding octets, they are equivalent. For consistency, URI            names in the IDNA encoding, rather than a percent-encoding,
   producers and normalizers should use uppercase hexadecimal digits             if they wish to maximize interoperability with legacy
   for all percent-encodings.                                                    URI resolvers.

   URIs that differ in the replacement of an unreserved character with         |
   its corresponding percent-encoding octet are equivalent: they identify      |
   the same resource. However, URI comparation implementations do not          |
   always perform normalization prior to comparison. For consistency,          |
   percent-encoding octets in the range of unreserved characters should        |
   not be created by URI producers and, when found in a URI, should be         |
   decoded to their corresponding unreserved characters by URI normalizers.    |
                                                                               |
   The purpose of reserved characters is to provide a set of delimiting        |
   characters that are distinguishable from other data wihin a URI. URIs       |
   that differ in the replacement of a reserved character with its             |
   corresponding percent-encoding octet are NOT equivalent. URI producing      |
   applications should percent-encode data octets that correspond to           |
   characters in the reserved set unless these characters are specifically     |
   allowed by the URI scheme to represent data in that component. If a         |
   reserved character is found in a URI component and no delimiting role is    |
   known for that character, then it must be interpreted as representing the   |
   data octet corresponding to that character's encoding in US-ASCII.          |
                                                                               |
   Because the percent ("%") character serves as the indicator for percent-    |
   encoding octets, it must be percent-encoded as "%25" for that octet to be   |
   used as data within a URI. Implementations must not percent-encode or       |
   decode the same string more than once, as decoding an already decoded       |
   string might lead to misinterpreting a percent data octet as the beginning  |
   of a percent-encoding, or vice versa in the case of percent-encoding an     |
   already percent-encoded string.                                             |
                                                                               |
   The request target URI have four distinct formats, depending on both the    |
   method being requested and whether the request is to a proxy. The           | asterisk-form request URI (only for OPTIONS method)
   asterisk-form of the request URI is only used for a server-wide OPTIONS     |
   request. When a client wishes to request OPTIONS for the server as a whole, |
   as opposed to a specific named resource of that server, the client MUST     |
   send only "*" as the request URI. For example `OPTIONS * HTTP/1.1`. If a    |
   proxy receives an OPTIONS request with an absolute-form of request URI in   |
   which the URI has an empty path and no query component, then the last proxy |
   on the request chain MUST send a request URI of "*" when it forwards the    |
   request to the indicated origin server.                                     |
                                                                               |
   When making a request directly to an origin server, other than a CONNECT    | origin-form request URI (for origin server)
   or server-wide OPTIONS request, a client MUST send only the absolute path   |
   and query components of the target URI as the request URI. If the target    |
   URI's path component is empty, the client MUST send "/" as the path within  |
   the origin-form of request URI. A Host header field is also sent.           |
                                                                               |
   The authority-form of request URI is only used for CONNECT requests. When   | authority-form request URI (only for CONNECT method)
   making a CONNECT request to establish a tunnel through one or more proxies, |
   a client MUST send only the targer URI's authority component (excluding     |
   any userinfo and its "@" delimiter) as the request URI. For example,        |
   `CONNECT www.example.com:80 HTTP/1.1`.                                      |
                                                                               |
   When making a request to a proxy, other than a CONNECT or server-wide       |
   OPTIONS request, a client MUST send the target URI in absolute-form as      | absolute-form request URI (for proxy, but server MUST also accept this form request)
   the request URI. The proxy is requested to either service that request      |
   from a valid cache, if possible, or make the same request on the client's   |
   behalf to either the next inbound proxy server or directly to the origin    |
   server indicated by the request URI. Requirements on such "forwarding" of   |
   messages are defined in Section 5.7. An example absolute-form of request-   |
   line would be: `GET http://www.example.org/pub/www/project.html HTTP/1.1`.  |
   To allow for transition to the absolute-form for all requests in some       |
   future version of HTTP, a server MUST accept the absolute-form in request,  |
   even though HTTP/1.1 clients will only send them in requests to proxies.    |
                                                                               |
   A client MUST send a Host header field in all HTTP/1.1 request messages. If | Host header field
   the target URI includes an authority component (authority-form or absolute- |
   form), then a client MUST send a field-value for Host that is identical to  |
   that authority component. If the authority component is missing or undefined|
   for the target URI, then a client MUST send a Host header field with an     |
   empty field-value. ???                                                      |
                                                                               |
   Since the Host field-value is critical information for handling a request,  |
   a user agent SHOULD generate Host as the first header field following the   |
   request-line. A client MUST send a Host header field in an HTTP/1.1 request |
   even if the request URI is in the absolute-form, since this allows the Host |
   information to be forwarded through ancient HTTP/1.0 proxies that might not |
   have implemented Host.                                                      |
                                                                               |
   When a proxy receives a request with an absolute-form of request URI, the   |
   proxy MUST ignore the received Host header field (if any) and instead       |
   replace it with the host information of the request URI. A proxy that       |
   forwards such a request MUST generate a new Host field-value based on the   |
   received request URI rather than forward the received Host field-value.     |
                                                                               |
   Since the Host header field acts as an application-level routing mechanism, |
   it is a frequent target for malware seeking to poison a shared cache or     |
   redirect a request to an unintended server. An interception proxy is        |
   particularly vulnerable if it relies on the Host field-value for            |
   redirecting requests to internal servers, or for use as a cache key in a    |
   shared cache, without first verifying that the intercepted connection is    |
   targeting a valid IP address for that host.                                 |
                                                                               |
   A server Must respond with a 400 (Bad Request) status code to any HTTP/1.1  |
   request message that lacks a Host header field and to any request message   |
   that contains more than one Host header field or an Host header field with  |
   an invalid field-value.                                                     |
*/

static void
l_http_read_requst_uri(lnlylib_env* E, l_http_accept_service* accept_srvc)
{
}

/* 1. A sender MUST NOT send whitespace between the start-line and the
   first header field.  A recipient that receives whitespace between the
   start-line and the first header field MUST either reject the message
   as invalid or consume each whitespace-preceded line without further
   processing of it (i.e., ignore the entire line, along with any
   subsequent lines preceded by whitespace, until a properly formed
   header field is received or the header section is terminated).

   The presence of such whitespace in a request might be an attempt to
   trick a server into ignoring that field or processing the line after
   it as a new request, either of which might result in a security
   vulnerability if other implementations within the request chain
   interpret the same message differently.  Likewise, the presence of
   such whitespace in a response might be ignored by some clients or
   cause others to cease parsing.

   2. A recipient MUST interpret a received protocol element according to
   the semantics defined for it by this specification, including
   extensions to this specification, unless the recipient has determined
   (through experience or configuration) that the sender incorrectly
   implements what is implied by those semantics.  For example, an
   origin server might disregard the contents of a received
   Accept-Encoding header field if inspection of the User-Agent header
   field indicates a specific implementation version that is known to
   fail on receipt of certain content codings.

   Unless noted otherwise, a recipient MAY attempt to recover a usable
   protocol element from an invalid construct.  HTTP does not define
   specific error handling mechanisms except when they have a direct
   impact on security, since different applications of the protocol
   require different error handling strategies.  For example, a Web
   browser might wish to transparently recover from a response where the
   Location header field doesn't parse according to the ABNF, whereas a
   systems control client might consider any form of error recovery to
   be dangerous.
*/

static void
l_http_read_headers(lnlylib_env* E)
{
}
