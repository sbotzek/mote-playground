#ifdef VERTEX
attribute vec4 a_position;
attribute vec4 a_color;

uniform mat4 u_model;
uniform mat4 u_view;
uniform mat4 u_projection;

varying lowp vec4 v_color;

void main(void) {
  gl_Position = u_projection * u_model * u_view * a_position;
  v_color = a_color;
}
#endif


#ifdef FRAGMENT
varying lowp vec4 v_color;

void main(void) {
  gl_FragColor = v_color;
}
#endif
