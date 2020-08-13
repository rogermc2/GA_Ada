#version 410 core

layout(location = 0) in vec3 vertex_position;
layout(location = 1) in vec3 vertex_normal;

//out vec3 Position_Worldspace;
out vec3 Camera_Normal;
//out vec3 Eye_Direction;
out vec3 Light_Direction;

uniform float line_width;
uniform mat4  view_matrix;
uniform mat4  model_matrix;
uniform mat4  projection_matrix;
uniform mat4  rotation_matrix;
uniform mat4  translation_matrix;
uniform vec3  light_position;
uniform vec3  light_direction;

void main()
    {
    mat4 tr_model_matrix = translation_matrix * rotation_matrix * model_matrix;
    mat4 mvp_matrix = projection_matrix * view_matrix * model_matrix;
    vec4 position = vec4(vertex_position, 1);
    vec4 normal = vec4(vertex_normal, 0);
    vec4 delta = vec4(vertex_normal * line_width, 0);
//    Position_Worldspace = (model_matrix * position).xyz;
      // Vector that goes from the vertex to the camera in camera space.
      // In camera space the camera is at the origin (0,0,0).
//    vec3 Vertex_Position_Camera_Space = (view_matrix * model_matrix * position).xyz;
//    Eye_Direction = vec3(0, 0, 0) - Vertex_Position_Camera_Space;
      // Vector that goes from the vertex to the light in camera space.
      // M is ommited because it's identity.
//    vec3 LightPosition = (view_matrix * vec4(light_position, 1)).xyz;  // Not used?
//    Light_Direction = Light_Direction + Eye_Direction;
    Light_Direction = light_direction;

    Camera_Normal = (view_matrix * model_matrix * normal).xyz;
    gl_Position = mvp_matrix * (position + delta);
    }
