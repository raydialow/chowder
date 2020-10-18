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

typedef uint32_t vkint;

const vkint chowder_version = VK_MAKE_VERSION(0, 0, 0);

VkApplicationInfo mk_application_info(const char* application_name, vkint application_version)
{
  VkApplicationInfo ret;
  ret.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;
  ret.pNext = NULL;
  ret.pApplicationName = application_name;
  ret.applicationVersion = application_version;
  ret.pEngineName = "Chowder";
  ret.engineVersion = chowder_version;
  ret.apiVersion = VK_API_VERSION_1_2;
  return ret;
}

GLFWwindow* mk_window(vkint width, vkint height, const char* title)
{
  glfwInit();
  glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API); //using Vulkan, not OpenGL
  glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
  return glfwCreateWindow(width, height, title, NULL, NULL);
}

VkInstanceCreateInfo mk_instance_create_info(VkApplicationInfo application_info,
                                             bool validation_layers)
{
  VkInstanceCreateInfo ret;

  vkint extension_count = 0;
  const char** extension_names = glfwGetRequiredInstanceExtensions(&extension_count);

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

VkInstance mk_instance(VkInstanceCreateInfo* instance_create_info)
{
  VkInstance ret;
  assert(vkCreateInstance(instance_create_info, NULL, &ret) == VK_SUCCESS);
  return ret;
}

vkint getno_physical_devices(VkInstance instance)
{
  vkint physical_device_count;
  vkEnumeratePhysicalDevices(instance, &physical_device_count, NULL);
  assert(physical_device_count > 0);
  return physical_device_count;
}

VkPhysicalDevice get_physical_device(VkInstance instance,
                                     vkint physical_device_index,
                                     vkint physical_device_count)
{
  assert(physical_device_count > physical_device_index);
  assert(physical_device_index >= 0);
  VkPhysicalDevice physical_devices[physical_device_count];
  vkEnumeratePhysicalDevices(instance, &physical_device_count, physical_devices);
  return physical_devices[physical_device_index];
}

VkPhysicalDeviceProperties get_physical_device_props(VkPhysicalDevice physical_device)
{
  VkPhysicalDeviceProperties physical_device_properties;
  vkGetPhysicalDeviceProperties(physical_device, &physical_device_properties);
  return physical_device_properties;
}

vkint get_physical_device_queue_family_count(VkPhysicalDevice physical_device)
{
  vkint ret;
  vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &ret, NULL);
  return ret;
}

VkDeviceCreateInfo mk_device_create_info(VkPhysicalDevice physical_device,
                                         vkint queue_family_count)
{
  assert(queue_family_count > 0);
  VkQueueFamilyProperties queue_family_props[queue_family_count];
  vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &queue_family_count, &queue_family_props[0]);

  VkDeviceQueueCreateInfo device_queue_create_infos[1];
  const float queue_priorities[] = {.5,.5,.5,.5};

  device_queue_create_infos[0].sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
  device_queue_create_infos[0].pNext = NULL;
  device_queue_create_infos[0].flags = 0;
  device_queue_create_infos[0].queueFamilyIndex = 0;
  device_queue_create_infos[0].queueCount = 4;
  device_queue_create_infos[0].pQueuePriorities = queue_priorities;

  bool queue_family_set = false;

  for(vkint iter = 0; iter < queue_family_count; iter += 1) {
    vkint queue_flags = queue_family_props[iter].queueFlags;
    vkint queue_count = queue_family_props[iter].queueCount;

    if(queue_flags&VK_QUEUE_GRAPHICS_BIT
       && queue_flags&VK_QUEUE_COMPUTE_BIT
       && queue_flags&VK_QUEUE_TRANSFER_BIT
       && queue_count >= 4) {
      device_queue_create_infos[0].queueFamilyIndex = iter;
      queue_family_set = true;
      break;
    }

  }

  assert(queue_family_set);

  VkPhysicalDeviceFeatures physical_device_features;
  vkGetPhysicalDeviceFeatures(physical_device, &physical_device_features);

  VkDeviceCreateInfo ret;
  ret.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
  ret.pNext = NULL;
  ret.flags = 0;
  ret.queueCreateInfoCount = 1;
  ret.pQueueCreateInfos = device_queue_create_infos;
  ret.enabledLayerCount = 0;
  ret.ppEnabledLayerNames = NULL;
  ret.enabledExtensionCount = 0;
  ret.ppEnabledExtensionNames = NULL;
  ret.pEnabledFeatures = &physical_device_features;

  return ret;
}

VkDevice mk_device(VkPhysicalDevice physical_device, VkDeviceCreateInfo device_create_info)
{
  VkDevice device;
  vkCreateDevice(physical_device, &device_create_info, NULL, &device);
  return device;
}

// set-up discrete command pools for each queue ...

// write vk convenience functions utilizing created queues

// write SDL2 convenience functions utilizing: Joystick, Audio,... etc.

// wire into scheme front end, utilize to build SDK

// ??? Profit

void chr_shutdown(VkInstance instance, GLFWwindow* window)
{
  vkDestroyInstance(instance, NULL);
  glfwTerminate();
}
