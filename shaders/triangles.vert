#version 410 core

layout(location = 0) in vec4 vPosition;
uniform mat4 MPMat;

void
main()
{
   gl_Position = MPMat * vPosition;
}
