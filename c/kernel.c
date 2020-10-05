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
#include <vulkan/vulkan.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_video.h>
#include <SDL2/SDL_vulkan.h>
#endif

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
  const char** extension_names;
  SDL_Vulkan_GetInstanceExtensions(window, &extension_count, extension_names);

  /* VALIDATION LAYERS UNUSED...
  uint32_t vlayers_count;
  if(validation_layers == true) {
	//do something else
	vlayers_count = 0;
  } else {
	vlayers_count = 0;
  }
  const char** vlayers_names;
  if(vlayers_count > 0) {
	//do something else
	vlayers_names = NULL;
  }
  */

  ret.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
  ret.pNext = NULL;
  ret.flags = 0;
  ret.pApplicationInfo = &application_info;
  ret.enabledLayerCount = 0; // vlayers
  ret.ppEnabledLayerNames = NULL; // vlayers
  ret.enabledExtensionCount = extension_count;
  ret.ppEnabledExtensionNames = extension_names;

  return ret;
}

VkInstance mk_instance(VkInstanceCreateInfo instance_create_info)
{
  VkInstance ret;
  assert(vkCreateInstance(&instance_create_info, NULL, &ret) == VK_SUCCESS);
  return ret;
}

VkPhysicalDevice get_physical_device(VkInstance instance)
{
  uint32_t physical_device_count;
  vkEnumeratePhysicalDevices(instance, &physical_device_count, NULL);
  assert(physical_device_count > 0);
  VkPhysicalDevice physical_devices[physical_device_count];
  vkEnumeratePhysicalDevices(instance, &physical_device_count, physical_devices);
  // query physical devices if there are multiple. if there is only one, return it.
  if(physical_device_count == 1) {
	return physical_devices[0];
  } else {
	// query...
	return physical_devices[0];
  }
}

void rm_instance(VkInstance instance, SDL_Window* window)
{
  vkDestroyInstance(instance, NULL);
  SDL_DestroyWindow(window);
  SDL_Quit();
}

int main()
{
  return 0;
}
