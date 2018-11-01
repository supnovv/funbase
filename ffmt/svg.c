#include "lnlylib.h"

#define SVGNS "http://www.w3.org/2000/svg"
#define XLINK "http://www.w3.org/1999/xlink"

/* <svg width="900" height="300" viewBox="0 0 300 100"
        xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
     <title>Image title</title>
     <desc>Image description</desc>
     <circle cx="10" cy="95" r="50" style="stroke:black; fill:none"/>
   </svg>
* viewBox="10 10 300 100" means width 900 should be mapped from 10 to 310,
  the height 300 should be mapped from 10 to 110.
* 如果 viewBox 指定的缩放比例与 width/height 不同，可用 preseveAspectRatio 指定对齐方式
* svg {
    shape-rendering: geometricPrecision;  // or crispEdges
  }
*/

typedef struct {
  l_strn name;
  l_int value;
} l_iattr;

typedef struct {
  l_strn name;
  double value;
} l_fattr;

typedef struct {
  l_strn name;
  l_strn value;
} l_sattr;

typedef struct {
  l_int tag;
  l_string id;
  l_string cl;
} l_tag;

typedef struct {
  l_tag head;
  l_iattr w;
  l_iattr h;
} l_svg;

typedef struct {
  l_tag head;
  l_string title;
  l_string desc;
} l_group;

/* <defs></defs> define images but not display them
   <use xlink:href="#id" transform="scale(-1 1) translate(-140 0)" /> */

typedef struct {
} l_use;

/* <image xlink:href="hello.jpg/png/gif" x="72" y="92" width="160" height=120"/> */

typedef struct {
  double r; /* 0 ~ 255 */
  double g; /* 0 ~ 255 */
  double b; /* 0 ~ 255 */
  double a; /* 0 ~ 1.0, opacity = 1.0 - alpha */
} l_color;

const static l_color l_none = {0x00, 0x00, 0x00};
const static l_color l_red = {0xff, 0x00, 0x00};
const static l_color l_green = {0x00, 0xff, 0x00};
const static l_color l_blue = {0x00, 0x00, 0xff};
const static l_color l_black = {0xff, 0xff, 0xff};

#define L_MAX_DASH_ARRAY_SIZE 8

typedef struct {
  double width;
  l_color color;
  l_int arr_size;
  double dash_arr[L_MAX_DASH_ARRAY_SIZE];
} l_stroke;

typedef struct {
  l_color color;
  l_int rule;
} l_color_fill;

typedef struct {
  l_string id;
  l_int rule;
} l_pattern_fill;

typedef struct {
  l_bool color_fill;
  union {
  l_color_fill color;
  l_pattern_fill patt;
  };
} l_fill;

typedef struct {
} l_transform;

/* <line x1="10" y1="10" x2="20" y2="20" style="stroke:black;"/> */

typedef enum {
  l_butt, l_round, l_square
} l_linecap;

typedef enum {
  l_miter, l_round, l_bevel
} l_linejoin;

typedef struct {
  l_stroke stroke;
  l_linecap linecap; /* butt round square */
  double x1;
  double y1;
  double x2;
  double y2;
} l_line;

/* <rect x="50" y="50" width="75" height="100"/> */

typedef struct {
  l_stroke stroke;
  l_fill fill;
  double x;
  double y;
  double w;
  double h;
  double rx; /* 0 ~ w/2, round angle of x-radius */
  double ry; /* 0 ~ h/2, round angle of y-radius */
} l_rect;

/* <circle cx="10" cy="10" r="50"/> */

typedef struct {
  l_stroke stroke;
  l_fill fill;
  double cx;
  double cy;
  double r;
} l_circle;

/* <ellipse cx="10" cy="10" rx="20" ry="30"/> */

typedef struct {
  l_stroke stroke;
  l_fill fill;
  double cx;
  double cy;
  double rx;
  double ry;
} l_ellipse;

/* <polyline points="100 62, 90 10, 70 45, 50, 10, 32, 62" style="stroke:black;"/> */

typedef struct {
  l_stroke stroke;
  l_fill fill;
  l_vector(l_point) points;
} l_polygon;

typedef struct {
  l_stroke stroke;
  l_linecap linecap;
  l_linejoin linejoin;
  double miter_limit; /* default is 4 */
  l_vector(l_point) points;
} l_polyline;

/* <path d="M 75 90 L 65 90 A 5 10 0 0 0 75 90" style="stroke:black;"/> */

typedef struct {
  l_font_family family;
  double size;
  l_bool bold;
  l_bool italic;
  l_text_decoration deco; /* none underline overline line-through */
  double word_spacing;
  double letter_spacing;
} l_font;

/* <text x="20" y="30">Simple text</text> */

typedef struct {
  l_text_anchor anchor; /* start middle end */
} l_text;

/* <tspan dy="3">like span element in html</tspan> */

L_EXTERN l_bool
lsvg_create_group(l_tag* g, const void* id)
{
  l_strn id_str = l_strn_c(id);
  if (id_str.p == 0 || id_str.n > L_MAX_TAG_ID_STR_SZ) {
    l_loge_s(LNUL, "invalud id of <g>");
    return false;
  }
  g->name[0] = 'g';
  g->name[1] = 0;
  l_copy_n(g->id, id_str.p, id_str.n);
  g->id[id_str.n] = 0;
  return true;
}

L_EXTERN l_bool
lsvg_create_use(l_use* use, const l_tag* tag, l_transform tf)
{
}

