#version 410

layout(location = 0) in vec3 vertex_position;

uniform mat4 MV_Matrix;
uniform mat4 Proj_Matrix;

void main()
{
  gl_Position = Proj_Matrix * MV_Matrix * vec4(vertex_position, 1);
}
