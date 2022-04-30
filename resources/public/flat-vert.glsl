
uniform mat4 viewMatrix;
uniform mat4 projectionMatrix;
uniform mat4 modelMatrix;
uniform mat4 cameraMatrix;
attribute vec4 position;
attribute vec3 normal;
uniform vec4 materialColor;
varying mediump vec4 fColor;

void main() {
    gl_Position = projectionMatrix * viewMatrix * cameraMatrix * modelMatrix * position;

    vec3 lightDirection = vec3(-3, -1, -1);
    vec4 vertexColor = vec4(0.0, 0.0, 0.0, 1.0);
    float ambientIntensity = 0.4;

    vec4 transformed_normal = cameraMatrix * modelMatrix * vec4(normal, 0.0);
    transformed_normal[3] = 0.0;
    transformed_normal = normalize(transformed_normal);

    vec3 halfPlane = normalize(vec3(0, 0, 1) - lightDirection);
    float nDotH = max(0.0, dot(transformed_normal.xyz, halfPlane));

    vertexColor += materialColor * ambientIntensity;
    vertexColor += materialColor * nDotH;
    vertexColor += vec4(1, 1, 1, 1) * nDotH * 0.05;

    fColor = vertexColor;
}