#version 410 core

layout(location = 0) in vec3 vertex_position;
layout(location = 1) in vec3 vertex_normal;

out vec3 Position_Worldspace;
out vec3 Camera_Normal;
out vec3 Eye_Direction;
out vec3 Light_Direction;

uniform mat4 view_matrix;
uniform mat4 model_matrix;
uniform mat4 mv_matrix;
uniform mat4 projection_matrix;
uniform vec3 light_position;

void main()
    {
    vec4 position = vec4(vertex_position, 1);
    vec4 normal = vec4(vertex_normal, 0);
    Position_Worldspace = (model_matrix * position).xyz;
      // Vector that goes from the vertex to the camera, in camera space.
      // In camera space, the camera is at the origin (0,0,0).
    vec3 Vertex_Position_Camera_Space = (view_matrix * model_matrix * position).xyz;
    Eye_Direction = vec3(0, 0, 0) - Vertex_Position_Camera_Space;
      // Vector that goes from the vertex to the light, in camera space. M is ommited because it's identity.
    vec3 LightPosition = (view_matrix * vec4(light_position, 1)).xyz;  // Not used?
    Light_Direction = Light_Direction + Eye_Direction;

    Camera_Normal = (view_matrix * model_matrix * normal).xyz;
        // Only correct if Model_Matrix does not scale the model !
        // Use its inverse transpose if not.
    gl_Position = projection_matrix * mv_matrix * position;
    }
