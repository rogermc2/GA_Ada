#version 410 core

//  Based on OGL_Turorials Tutorial 08_basic_shading
// Interpolated values from the vertex shaders
in vec3 Position_Worldspace;
in vec3 Camera_Normal;
in vec3 Eye_Direction;
in vec3 Light_Direction;

// Ouput data
out vec3 colour;

// Values that stay constant for the whole mesh.
uniform sampler2D myTextureSampler;
uniform mat4 MV;
uniform vec3 Light_Position_Worldspace;
uniform vec4 Ambient_Colour;
uniform vec4 Diffuse_Colour;
uniform vec4 Drawing_Colour;

void main()
{
    float LightPower = 50.0f;
    // Distance to the light
    float distance = length(Light_Position_Worldspace - Position_Worldspace);
    
    // Normal of the computed fragment, in camera space
    vec3 norm = normalize(Camera_Normal);
    // Direction of the light (from the fragment to the light)
    vec3 light_dir = normalize(Light_Direction);
    // Cosine of the angle between the normal and the light direction,
    // clamped above 0
    //  - light is at the vertical of the triangle -> 1
    //  - light is perpendicular to the triangle -> 0
    //  - light is behind the triangle -> 0
    float cosTheta = clamp(dot(norm, light_dir), 0, 1);
    
    vec3 LightColour = (Drawing_Colour + Ambient_Colour + Diffuse_Colour * LightPower * cosTheta / (distance*distance)).xyz;
    colour = LightColour;
}
