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

#define _KRNL_PREPRO_
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>
#define BUFFER_SIZE_X_SMALL 0x0100
#define BUFFER_SIZE_SMALL   0x0400
#define BUFFER_SIZE_MEDIUM  0x0800
#define BUFFER_SIZE_LARGE   0x0c00
#define BUFFER_SIZE_X_LARGE 0x1000

/*
 * Typedefs
 */
typedef uint32_t vkint;

/*
 * Constants
 */
const vkint chowder_version_major       = 0x0;
const vkint chowder_version_minor       = 0x0;
const vkint chowder_version_rev         = 0x0;
const vkint innerlands_version_major    = 0x0;
const vkint innerlands_version_minor    = 0x0;
const vkint innerlands_version_rev      = 0x0;

const char* chowder_name        = "Chowder";
const char* innerlands_name     = "Innerlands";
const vkint chowder_version     = VK_MAKE_VERSION(chowder_version_major,
                                                  chowder_version_minor,
                                                  chowder_version_rev);
const vkint innerlands_version  = VK_MAKE_VERSION(innerlands_version_major,
                                                  innerlands_version_minor,
                                                  innerlands_version_rev);

const vkint queue_count = 4;
const float queue_priorities[] = {.5,.5,.5,.5};

/*
 * MAIN
 */
int main()
{
    /*
     * Declaration
     */
    VkApplicationInfo               application_info;
    GLFWwindow*                     window;
    vkint                           extension_count;
    const char**                    extension_names;
    VkInstanceCreateInfo            instance_create_info;
    VkInstance                      instance;
    VkSurfaceKHR                    surface;
    vkint                           physical_device_count;
    VkPhysicalDevice                physical_device;
    VkPhysicalDeviceProperties      physical_device_properties;
    VkPhysicalDeviceFeatures        physical_device_feats;
    vkint                           queue_family_count;
    VkDeviceQueueCreateInfo         device_queue_create_info[1];
    bool                            queue_family_set;
    vkint                           queue_family_index;
    VkDeviceCreateInfo              device_create_info;
    VkDevice                        device;
    /* Device Queues */
    //VkQueue                       queue_2d;
    //VkQueue                       queue_3d;
    //VkQueue                       queue_tc;
    //VkQueue                       queue_rp;

    /*
     * Initialization
     */
    glfwInit();
    glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API); //using Vulkan
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);

    application_info.sType                          = VK_STRUCTURE_TYPE_APPLICATION_INFO;
    application_info.pNext                          = NULL;
    application_info.pApplicationName               = innerlands_name;
    application_info.applicationVersion             = innerlands_version;
    application_info.pEngineName                    = chowder_name;
    application_info.engineVersion                  = chowder_version;
    application_info.apiVersion                     = VK_API_VERSION_1_2;

    window                                          = glfwCreateWindow(800, 600, innerlands_name, NULL, NULL);
    extension_count                                 = 0;
    extension_names                                 = glfwGetRequiredInstanceExtensions(&extension_count);

    instance_create_info.sType                      = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
    instance_create_info.pNext                      = NULL;
    instance_create_info.flags                      = 0;
    instance_create_info.pApplicationInfo           = &application_info;
    instance_create_info.enabledLayerCount          = 0; // vlayers
    instance_create_info.ppEnabledLayerNames        = NULL; // vlayers
    instance_create_info.enabledExtensionCount      = extension_count;
    instance_create_info.ppEnabledExtensionNames    = extension_names;

    assert( vkCreateInstance(&instance_create_info, NULL, &instance) == VK_SUCCESS );
    assert( glfwCreateWindowSurface(instance, window, NULL, &surface) == VK_SUCCESS);
    assert( vkEnumeratePhysicalDevices(instance, &physical_device_count, NULL) == VK_SUCCESS );
    assert( physical_device_count > 0 );
    VkPhysicalDevice physical_devices[physical_device_count];
    assert( vkEnumeratePhysicalDevices(instance, &physical_device_count, physical_devices) == VK_SUCCESS );

    /* Grabbing First Device by Default */
    physical_device                                 = physical_devices[0];

    vkGetPhysicalDeviceProperties(physical_device, &physical_device_properties); //void
    vkGetPhysicalDeviceFeatures(physical_device, &physical_device_feats); //void
    vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &queue_family_count, NULL); //void
    assert( queue_family_count > 0 );
    VkQueueFamilyProperties queue_family_props[queue_family_count];
    vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &queue_family_count, &queue_family_props[0]); //void

    /* Assure Queue Family has graphics, compute, and transfer bits set, can hold at least 4 queues, and has surface support */
    queue_family_set = false;
    for(vkint iter = 0; iter < queue_family_count; iter += 1) {
        vkint surface_support = 0;
        vkint queue_flags = queue_family_props[iter].queueFlags;
        vkint queue_max = queue_family_props[iter].queueCount;
        vkGetPhysicalDeviceSurfaceSupportKHR(physical_device, iter, surface, &surface_support);
        if(queue_flags&VK_QUEUE_GRAPHICS_BIT
           && queue_flags&VK_QUEUE_COMPUTE_BIT
           && queue_flags&VK_QUEUE_TRANSFER_BIT
           && queue_max >= queue_count
           && surface_support) {
            queue_family_index                      = iter;
            queue_family_set                        = true;
            break;
        }
    }
    assert( queue_family_set );

    device_queue_create_info[0].sType               = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
    device_queue_create_info[0].pNext               = NULL;
    device_queue_create_info[0].flags               = 0;
    device_queue_create_info[0].queueFamilyIndex    = queue_family_index; // temporary assignment
    device_queue_create_info[0].queueCount          = 4;
    device_queue_create_info[0].pQueuePriorities    = queue_priorities;

    device_create_info.sType                        = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
    device_create_info.pNext                        = NULL;
    device_create_info.flags                        = 0;
    device_create_info.queueCreateInfoCount         = 1;
    device_create_info.pQueueCreateInfos            = device_queue_create_info;
    device_create_info.enabledLayerCount            = 0;
    device_create_info.ppEnabledLayerNames          = NULL;
    device_create_info.enabledExtensionCount        = 0;
    device_create_info.ppEnabledExtensionNames      = NULL;
    device_create_info.pEnabledFeatures             = &physical_device_feats;

    assert( vkCreateDevice(physical_device, &device_create_info, NULL, &device) == VK_SUCCESS );

/* TODO: Need a
 * Swapchain,
 * Imageviews,
 * Graphics Pipeline,
 * Framebuffers,
 * Command Pool and Buffers,
 * Render Loop,
 * <AUDIO STUFF>,
 * <CONTROLS STUFF>,
 * <SCRIPTING STUFF>
 */


    /*
     * Procedure
     */
    printf("Started Vulkan instance using device %s with four queues on family index %u.\n",
           physical_device_properties.deviceName,
           queue_family_index);
    vkDestroySurfaceKHR(instance, surface, NULL);
    vkDestroyInstance(instance, NULL);
    glfwDestroyWindow(window);
    glfwTerminate();
    puts("Terminated cleanly.\n");
    return 0;
}
