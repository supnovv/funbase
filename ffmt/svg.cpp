#include "ffmt/svg.h"

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

#define L_SVG_DOC 0
#define L_SVG_DEFS 1
#define L_SVG_GROUP 2
#define L_SVG_USE 3
#define L_SVG_IMG 4
#define L_SVG_PLAIN 5
#define L_SVG_TEXT 6
#define L_SVG_TSPAN 7
#define L_SVG_PATH 8
#define L_SVG_LINE 9
#define L_SVG_RECT 10
#define L_SVG_CIRCLE 11
#define l_SVG_ELLIPSE 12
#define L_SVG_POLYGON 13
#define L_SVG_POLYLINE 14

class l_docnode: public l_treenode {
public:
  l_string id;
  l_string class_list;

  l_docnode(l_uint node_ud) {
    l_treenode_init(&doc->node, node_ud);
  }

  void set_id(l_strn id) {
    this->id.reset(id);
  }

  void set_class(l_strn class_list) {
    this->class_list.reset(class_list);
  }

  void add_class(l_strn class_name) {
    if (class_list.empty()) {
      class_list.append(class_name);
    } else {
      l_ostream* os = class_list.ostream();
      l_ostream_format_1(os, ".%strn", &class_name);
    }
  }

  void del_class(l_strn class_name) {
  }
};

class l_point {
public:
  double x;
  double y;

  l_point(): x(0), y(0) {}

  l_point(double a, double b):
    x(a), y(b)
    {}
};

class l_vec {
public:
  double x;
  double y;

  l_vec(): x(0), y(0) {}

  l_vec(double a, double b):
    x(a), y(b)
    {}
};

class l_size {
public:
  double w;
  double h;

  l_size(): w(0), y(0) {}

  l_size(double width, double height):
    w(width), h(height)
    {}
};

class l_svgdoc: public l_docnode {
public:
  l_point pos;
  l_size size;
  l_vec scale;
  l_string title;
  l_string desc;

  l_svgdoc(l_size size, const void* svg_title, const void* svg_desc):
    l_docnode(L_SVG_DOC),
    size(l_size(size.w > 0 size.w : 0, size.h > 0 ? size.h : 0)),
    title(l_string(0, l_strn_c(svg_title))),
    desc(l_string(0, l_strn_c(svg_desc)))
    {}

  void set_pos(l_point pos) {
    this->pos = pos;
  }

  void set_scale(double scale) {
    this->scale = l_vec(scale, scale);
  }

  void set_scale_x(l_vec scale) {
    this->scale = scale;
  }
};

class l_svgdefs: public l_docnode { /* define graphes but not display them */
public:
  l_svgdefs():
    l_docnode(L_SVG_DEFS)
    {}
};

class l_svggroup: public l_docnode {
public:
  l_string title;
  l_string desc;

  l_svggroup(const void* group_title, const void* group_desc):
    l_docnode(L_SVG_GROUP),
    title(l_string(0, l_strn_c(group_title))),
    desc(l_string(0, l_strnc(group_desc)))
    {}

};

class l_svguse: public l_docnode { /* <use xlink:href="#id" transform="scale(-1 1) translate(-140 0)" /> */
public:
  l_string graph_id;
  l_vec scale;
  l_vec translate;
  double rotate;

  l_svguse(l_strn id):
    l_docnode(L_SVG_USE),
    graph_id(l_string(0, id)),
    rotate(0)
    {}

  l_svguse& set_scale(double scale) {
    this->scale = l_vec(scale, scale);
    return *this;
  }

  l_svguse& set_scale_x(l_vec scale) {
    this->scale = scale;
    return *this;
  }

  l_svguse& set_translate(l_vec translate) {
    this->translate = translate;
    return *this;
  }

  l_svguse& set_rotate(double rotate) {
    this->rotate = rotate;
    return *this;
  }
};

class l_color {
public:
  double r; /* 0 ~ 255 */
  double g; /* 0 ~ 255 */
  double b; /* 0 ~ 255 */
  double a; /* 0 ~ 1.0, opacity = 1.0 - alpha */

  l_color():
    r(0), g(0), b(0), a(0)
    {}

  l_color(double red, double green, double blue):
    r(red), g(green), b(blue), a(0)
    {}

  l_color(double red, double green, double blue, double alpha):
    r(red), g(green), b(blue), a(alpha)
    {}

  const static l_color red = l_color(0xff, 0x00, 0x00);
  const static l_color green = l_color(0x00, 0xff, 0x00);
  const static l_color blue = l_color(0x00, 0x00, 0xff);
  const static l_color white = l_color(0xff, 0xff, 0xff);

  const static l_color code_blue = l_color(0, 92, 197);
  const static l_color code_navy = l_color(3, 47, 98);
  const static l_color code_red = l_color(215, 58, 73);
  const static l_color code_purple = l_color(111, 66, 193);
  const static l_color code_green = l_color(34, 134, 58);
  const static l_color code_yellow = l_color(227, 98, 9);
  const static l_color code_grey = l_color(106, 115, 125);
  const static l_color code_black = l_color(36, 41, 46);
};


class l_stroke {
  enum {max_dashes = 4};
  l_int arr_size;
  double dash_arr[max_dashes];
public:
  double width;
  l_color color;

  l_stroke():
    arr_size(0),
    width(1),
    color(l_color::black())
    {}

  l_stroke& set_width(double width) {
    this->width = width;
    return *this;
  }

  l_stroke& set_color(l_color color) {
    this->color = color;
    return *this;
  }

  l_stroke& set_dash() {
    arr_size = 0;
    return *this;
  }

  l_stroke& set_dash(double a, double b) {
    arr_size = 2;
    dash_arr[0] = a;
    dash_arr[1] = b;
    return *this;
  }

  l_stroke& set_dash(double a, double b, double c, double d) {
    arr_size = 4;
    dash_arr[0] = a;
    dash_arr[1] = b;
    dash_arr[2] = c;
    dash_arr[3] = d;
    return *this;
  }
};


class l_fillrule {
  l_bool pattern_fill;
  l_byte rule;
  l_color color;
  l_string graph_id;
public:
  l_fillrule():
    pattern_fill(false),
    rule(0),
    {}

  l_fillrule(l_color fill_color):
    pattern_fill(false),
    rule(0),
    color(fill_color)
    {}

  l_fillrule(l_color fill_color, l_byte fill_rule):
    pattern_fill(false),
    rule(fill_rule),
    color(fill_color)
    {}

  l_fillrule(l_strn graph_id):
    pattern_fill(true),
    rule(0),
    graph_id(0, graph_id)
    {}

  l_fillrule(l_strn graph_id, l_byte fill_rule):
    pattern_fill(true),
    rule(fill_rule),
    graph_id(0, graph_id)
    {}
};

class l_font {
  l_string family; /* serif sans-serif monospace */
  double size;
  l_bool bold;
  l_bool italic;
  l_byte decoration; /* none underline overline line-trough */
  l_byte anchor; /* default start(left) middle end(right) */
  double word_spacing;
  double letter_spacing;
  double line_height;
public:
  l_font() {
    l_zero_n(this, sizeof(l_font));
  }

  static l_font code_font() const {
    l_font font;
    font.set_family(l_literal_strn("SFMono-Regular, Consolas, Liberation Mono, Menlo, Courier, monospace"));
    font.set_size(12);
    font.set_line_height(20);
    return font;
  }

};

class l_linecap {
  l_int type;
  l_linecap(l_int cap_type): type(cap_type) {}
public:
  l_linecap(): type(0) {}

  const static l_linecap butt = l_linecap(0);
  const static l_linecap round = l_linecap(1);
  const static l_linecap square = l_linecap(2);
};

class l_linejoin {
  l_int type;
  l_linejoin(l_int join_type): type(join_type) {}
public:
  l_linejoin(): type(0) {}

  const static l_linejoin mitter = l_linejoin();
  const static l_linejoin round = l_linejoin(1);
  const static l_linejoin bevel = l_linejoin(2);
};

/* <line x1="10" y1="10" x2="20" y2="20" style="stroke:black;"/> */

class l_svgline: public l_docnode {
public:
  l_stroke stroke;
  l_linecap cap;
  l_point start;
  l_point end;
};

/* <rect x="50" y="50" width="75" height="100"/> */

class l_svgrect: public l_docnode {
public:
  l_stroke stroke;
  l_fillrule fill;
  l_point pos;
  l_size size;
  double rx; /* 0 ~ w/2, round angle of x-radius */
  double ry; /* 0 ~ h/2, round angle of y-radius */
};

/* <circle cx="10" cy="10" r="50"/> */

class l_svgcircle: public l_docnode {
public:
  l_stroke stroke;
  l_fill fill;
  double cx;
  double cy;
  double r;
};

/* <ellipse cx="10" cy="10" rx="20" ry="30"/> */

class l_svgellipse: public l_docnode {
public:
  l_stroke stroke;
  l_fill fill;
  double cx;
  double cy;
  double rx;
  double ry;
};

/* <polyline points="100 62, 90 10, 70 45, 50, 10, 32, 62" style="stroke:black;"/> */

class l_svgpolygon: public l_docnode {
public:
  l_stroke stroke;
  l_fill fill;
  l_vector(l_point) points;
};

class l_svgpolyline: public l_docnode {
public:
  l_stroke stroke;
  l_linecap linecap;
  l_linejoin linejoin;
  double miter_limit; /* default is 4 */
  l_vector(l_point) points;
};

/**
<path d="M 75 90 L 65 90 A 5 10 0 0 0 75 90" style="stroke:black;"/>
M (moveto), a path need start with M, and can contain multiple M
L (lineto)
m and l use relative point to previous one
Z/z means lineto the start point of this path
H/h draw a horizontal line
V/v draw a vertial line
L/l can have multiple points
M/m can also have multiple points, the point start from the 2nd means lineto
the blanks can be remove between letters and numbers, between letters/numbers and negative signs
A/a draw a ellipse curve to point (x, y) - A rx ry x-axis-rotation large-arc draw-clockwise x y
Q/q draw a quadratic bezier curve to point (x, y) - Q control-point-x control-point-y x y
T/t draw a smooth quadratic bezier curve based on previous control point - M30 100Q80 30 100 100T200 30
C/c draw a cubic bezier curve to point (x, y) - C start-control-point-x start-control-point-y end-control-point-x end-control-point-y x y
S/s draw a smooth cubic bezier curve based on previous control point - S end-control-point-x end-control-point-y x y
**/

class l_svgpath: public l_docnode {
public:
};


/**
<text x="20" y="30">Simple text</text>
baseline, the bottom of A or X
descent, the height below baseline, i.e. to the bottom of character like g
ascent, the height above baseline, including the height of A and tone character height above A
font height em = descent + ascent, x-height < cap-height (upper letter height) < ascent < em
font-family, common font (serif sans-serif monospace fantasy cursive) should put in the tail

Code - blue rgb(0 92 197)
     - navy rgb(3 47 98)
     - red rgb(215 58 73)
     - purple rgb(111 66 193)
     - green rgb(34 134 58)
     - yellow rgb(227 98 9)
     - grey rgb(106 115 125)
     - black rgb(36 41 46)
     - font-size 12px
     - line-height 20px
     - font-family SFMono-Regular, Consolas, Liberation Mono, Menlo, Courier, monospace

<tspan x="10" y="10" dx="1" dy="2" transform="rotate(90)" writing-mode="tb" glyph-orientation-vertical="0"></tspan>
SVG doesn't process newline automatically, tspan can be used to locate newline position
dx/dy is still effective after tspan element end
but baseline-shift="super or sub" only effective inside tspan
transform, writing-mode, and glyph-orientation-vertical can used to control text's direction
textLength is used to set text length, the letter space is adjusted if it is too short, it is truncated if too long
SVG's blank character handling - ignore newlines, change tab to space, replace continuesly spaces to one space

<textPath xlink:href="#path-id" startOffset="50%" textAnchor="middle"></textPath>
character's baseline is adjusted as curve's tangent line
*/

class l_plain: public l_docnode {
public:
  l_string text;

  l_plain():
    l_docnode(L_SVG_PLAIN)
    {}
};

class l_svgtext: public l_docnode {
public:
};

