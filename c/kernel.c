/*
 *  kernel.c --- the Chowder game engine kernel
 *
 *  Copyright 2020 June Sage Rana
 *
 *  This program is free software: you can redistribute it and/or modify
 *
 *  it under the terms of the fuck around and find out license v0.1 as
 *  published in this program.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 *  You should have received a copy of the the fuck around and find out
 *  license v0.1 along with this program.  If not, see
 *  <https://paste.sr.ht/blob/d581b82a39d6f36f2f4c541785cee349b2549699>.
 *
 */

#ifndef _KRNL_INCLUDES_
#define _KRNL_INCLUDES_
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>
#endif

#include "kernel.h"

struct chrQueues {
  VkQueue q2D; // 2D queue for 2d fx, particles, ui, etc.
  VkQueue qTC; // TraceCollision queue for collisions and ray tracing comps, etc.
  VkQueue q3D; // 3D queue for geometry ops, tesselation, etc.
  VkQueue qRP; // RenderPipeline queue for shaders, raster, render, etc.
} chrQueues;

int main()
{
  // start up
  const char* application_name = "Unnamed Application";
  const vkint application_version = VK_MAKE_VERSION(0, 0, 0);
  VkApplicationInfo application_info = mk_application_info(application_name, application_version);
  vkint width = 800;
  vkint height = 600;
  const char* window_title = "Chowder v0.0.0";

  puts("Making window.");
  GLFWwindow* window = mk_window(width, height, window_title);
  bool validation_layers = false;
  VkInstanceCreateInfo instance_create_info;

  puts("Making instance create info.");
  instance_create_info = mk_instance_create_info(application_info, validation_layers);

  puts("Making instance.");
  VkInstance instance = mk_instance(&instance_create_info);

  // shut down
  puts("Shutting down.");
  chr_shutdown(instance, window);
  return 0;
}
