#version 410 core

layout(location = 0) in vec2 vertex;
uniform vec2 dimensions;
uniform mat4 mvp_matrix;

out vec2 texture_coords;

void main()
{
 //   gl_Position = projection_matrix * vec4(vertex.xy, 0.0, 1.0);
 //   texture_coords = vec2(vertex.zw);
    gl_Position = mvp_matrix * vec4(vertex.x * dimensions.x, vertex.y * dimensions.y, 0.0, 1.0);
    texture_coords = vertex;
}

