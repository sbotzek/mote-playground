#ifdef VERTEX
attribute vec4 a_position;
attribute vec2 a_textureCoord;

uniform mat4 u_model;
uniform mat4 u_view;
uniform mat4 u_projection;

varying highp vec2 v_textureCoord;

void main(void) {
  gl_Position = u_projection * u_model * u_view * a_position;
  v_textureCoord = a_textureCoord;
}
#endif


#ifdef FRAGMENT
varying highp vec2 v_textureCoord;

uniform sampler2D u_texture;

void main(void) {
  gl_FragColor = texture2D(u_texture, v_textureCoord);
}
#endif
