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

#include <vulkan/vulkan.h>

const uint32_t chowder_version = VK_MAKE_VERSION(0, 0, 0);

VkApplicationInfo make_application_info(uint32_t maj, uint32_t min, uint32_t rev)
{
  VkApplicationInfo ret;
  ret.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;
  ret.pNext = NULL;
  ret.pApplicationName = "Unnamed";
  ret.applicationVersion = VK_MAKE_VERSION(maj, min, rev);
  ret.pEngineName = "Chowder";
  ret.engineVersion = chowder_version;
  ret.apiVersion = VK_API_VERSION_1_2;
  return ret;
}
