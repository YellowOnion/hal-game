#version 330 core

in vec3 Normal;
in vec3 FragPos;

out vec4 FragColor;

void main()
{
  vec3 lightPos = vec3(-3.0f, 10.0f, 10.0f);
  vec3 lightDir = normalize(lightPos - FragPos);
  float diff = max((dot(Normal, lightDir) + 0.0) *1, 0.1);

  FragColor = vec4(diff * vec3(1.0f, 0.9f, 0.8f), 1.0f);
  //FragColor = vec4(max(gl_FragCoord.z, 0) * vec3(1.0f, 0.5f, 0.2f), 1.0f);
}
