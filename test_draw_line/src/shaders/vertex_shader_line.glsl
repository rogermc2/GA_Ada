#version 410 core

layout(location = 0) in vec3 vertex_position;

uniform mat4  mv_matrix;

void main()
{
  vec4 position = vec4(vertex_position, 1);
  // gl_PointSize = 40.0;
  // gl_Position = vec4(0.0, 0.0, 0.5, 1.0);
  gl_Position = mv_matrix * position;
}
