/*
 *  kernel.h --- the Chowder game engine kernel header
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

#include <assert.h>
#include <stdbool.h>
#include <vulkan/vulkan.h>
#include <SDL/SDL.h>
#include <SDL/SDL_video.h>
#include <SDL/SDL_vulkan.h>

const uint32_t chowder_version = VK_MAKE_VERSION(0, 0, 0);

VkApplicationInfo mk_application_info(uint32_t application_version)
{
  VkApplicationInfo ret;
  ret.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;
  ret.pNext = NULL;
  ret.pApplicationName = "Unnamed";
  ret.applicationVersion = application_version;
  ret.pEngineName = "Chowder";
  ret.engineVersion = chowder_version;
  ret.apiVersion = VK_API_VERSION_1_2;
  return ret;
}

SDL_Window* mk_window(uint32_t width, uint32_t height, bool fullscreen)
{
  SDL_Init(SDL_INIT_EVERYTHING);
  uint32_t flags;
  if(fullscreen == true) {
	flags = SDL_WINDOW_VULKAN|SDL_WINDOW_FULLSCREEN;
  } else {
	flags = SDL_WINDOW_VULKAN;
  }
  return SDL_CreateWindow("Chowder", 0, 0, width, height, flags);
}

VkInstanceCreateInfo mk_instance_create_info(VkApplicationInfo application_info,
											 SDL_Window* window,
											 bool validation_layers)
{
  VkInstanceCreateInfo ret;

  uint32_t extension_count;
  SDL_Vulkan_GetInstanceExtensions(window, &extension_count, NULL);
  char** extension_names;
  SDL_Vulkan_GetInstanceExtensions(window, &extension_count, extension_names);

  uint32_t vlayers_count = 0;
  char ** vlayers_names = NULL;
  if(validation_layers == true) {
	//do something else
  }

  ret.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
  ret.pNext = NULL;
  ret.flags = 0;
  ret.pApplicationInfo = application_info;
  ret.enabledLayerCount = vlayers_count;
  ret.ppEnabledLayerNames = vlayers_names;
  ret.enabledExtensionCount = extension_count;
  ret.ppEnabledExtensionNames = extension_names;

  return ret;
}

VkInstance mk-instance(VkInstanceCreateInfo instance_create_info)
{
  VkInstance ret;
  assert(vkCreateInstance(instance_create_info, NULL, &ret) == VK_SUCCESS);
  return ret;
}

void rm-instance(VkInstance instance, SDL_Window* window)
{
  vkDestroyInstance(instance, NULL);
  SDL_DestroyWindow(window);
  SDL_Quit();
}
