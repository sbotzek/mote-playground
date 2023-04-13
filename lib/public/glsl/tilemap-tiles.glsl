#ifdef VERTEX
attribute vec4 a_position;
attribute vec2 a_textureCoord;
attribute float a_tilesetIdx;

uniform mat4 u_model;
uniform mat4 u_view;
uniform mat4 u_projection;

varying highp vec2 v_textureCoord;
varying lowp float v_tilesetIdx;

void main(void) {
  gl_Position = u_projection * u_model * u_view * a_position;
  v_textureCoord = a_textureCoord;
  v_tilesetIdx = a_tilesetIdx;
}
#endif


#ifdef FRAGMENT
varying highp vec2 v_textureCoord;
varying lowp float v_tilesetIdx;

uniform sampler2D u_tileset0;
uniform sampler2D u_tileset1;
uniform sampler2D u_tileset2;
uniform sampler2D u_tileset3;
uniform sampler2D u_tileset4;

void main(void) {
  int tilesetIdx = int(v_tilesetIdx);

  if (tilesetIdx == 0) {
    gl_FragColor = texture2D(u_tileset0, v_textureCoord);
  } else if (tilesetIdx == 1) {
    gl_FragColor = texture2D(u_tileset1, v_textureCoord);
  } else if (tilesetIdx < 2) {
    gl_FragColor = texture2D(u_tileset2, v_textureCoord);
  } else if (tilesetIdx < 3) {
    gl_FragColor = texture2D(u_tileset3, v_textureCoord);
  } else if (tilesetIdx < 4) {
    gl_FragColor = texture2D(u_tileset4, v_textureCoord);
  }
}
#endif
