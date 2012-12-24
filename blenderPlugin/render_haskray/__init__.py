bl_info = {
    "name": "HaskRay Render",
    "author": "Tom Savage",
    "version": (0, 0, 1),
    "blender": (2, 5, 7),
    "location": "Render > Engine > HaskRay",
    "description": "Simple HaskRay integration",
    "warning": "experimental",
    "category": "Render"}

if "bpy" in locals():
    import imp
    imp.reload(ui)
    imp.reload(render)
    imp.reload(update_files)

else:
    import bpy
    from bpy.props import StringProperty, BoolProperty, IntProperty, FloatProperty, \
                          FloatVectorProperty, EnumProperty, PointerProperty
    from . import ui
#    from . import render
#    from . import update_files

class RenderHaskRaySettingsScene(bpy.types.PropertyGroup):
    # File Options
    tempfiles_enable = BoolProperty(
            name="Enable Tempfiles",
            description="Enable the OS-Tempfiles. Otherwise set the path where to save the files",
            default=True)
    deletefiles_enable = BoolProperty(
            name="Delete files",
            description="Delete files after rendering. Doesn't work with the image",
            default=True)
    scene_name = StringProperty(
            name="Scene Name",
            description="Name of POV-Ray scene to create. Empty name will use the name of " \
                        "the blend file",
            default="", maxlen=1024)
    scene_path = StringProperty(
            name="Export scene path",
            # description="Path to directory where the exported scene (POV and INI) is created",  # Bug in POV-Ray RC3
            description="Path to directory where the files are created",
            default="", maxlen=1024, subtype="DIR_PATH")
    renderimage_path = StringProperty(
            name="Rendered image path",
            description="Full path to directory where the rendered image is saved",
            default="", maxlen=1024, subtype="DIR_PATH")
    list_lf_enable = BoolProperty(
            name="LF in lists",
            description="Enable line breaks in lists (vectors and indices). Disabled: " \
                        "lists are exported in one line",
            default=True)

    # Not a real pov option, just to know if we should write
    radio_enable = BoolProperty(
            name="Enable Radiosity",
            description="Enable POV-Rays radiosity calculation",
            default=False)
    radio_display_advanced = BoolProperty(
            name="Advanced Options",
            description="Show advanced options",
            default=False)
    media_enable = BoolProperty(
            name="Enable Media",
            description="Enable POV-Rays atmospheric media",
            default=False)
    media_samples = IntProperty(
            name="Samples",
            description="Number of samples taken from camera to first object " \
                        "encountered along ray path for media calculation",
            min=1, max=100, default=35)

    media_color = FloatVectorProperty(
            name="Media Color", description="The atmospheric media color",
            precision=4, step=0.01, min=0, soft_max=1,
            default=(0.001, 0.001, 0.001), options={'ANIMATABLE'}, subtype='COLOR')

    baking_enable = BoolProperty(
            name="Enable Baking",
            description="Enable POV-Rays texture baking",
            default=False)
    indentation_character = EnumProperty(
            name="Indentation",
            description="Select the indentation type",
            items=(("0", "None", "No indentation"),
                   ("1", "Tabs", "Indentation with tabs"),
                   ("2", "Spaces", "Indentation with spaces")),
            default="2")
    indentation_spaces = IntProperty(
            name="Quantity of spaces",
            description="The number of spaces for indentation",
            min=1, max=10, default=4)

    comments_enable = BoolProperty(
            name="Enable Comments",
            description="Add comments to pov file",
            default=True)

    # Real pov options
    command_line_switches = StringProperty(
            name="Command Line Switches",
            description="Command line switches consist of a + (plus) or - (minus) sign, followed " \
                        "by one or more alphabetic characters and possibly a numeric value",
            default="", maxlen=500)

    antialias_enable = BoolProperty(
            name="Anti-Alias", description="Enable Anti-Aliasing",
            default=True)

    antialias_method = EnumProperty(
            name="Method",
            description="AA-sampling method. Type 1 is an adaptive, non-recursive, super-sampling "\
                        "method. Type 2 is an adaptive and recursive super-sampling method",
            items=(("0", "non-recursive AA", "Type 1 Sampling in POV-Ray"),
                   ("1", "recursive AA", "Type 2 Sampling in POV-Ray")),
            default="1")

    antialias_depth = IntProperty(
            name="Antialias Depth", description="Depth of pixel for sampling",
            min=1, max=9, default=3)

    antialias_threshold = FloatProperty(
            name="Antialias Threshold", description="Tolerance for sub-pixels",
            min=0.0, max=1.0, soft_min=0.05, soft_max=0.5, default=0.1)

    jitter_enable = BoolProperty(
            name="Jitter",
            description="Enable Jittering. Adds noise into the sampling process (it should be " \
                        "avoided to use jitter in animation)",
            default=True)

    jitter_amount = FloatProperty(
            name="Jitter Amount", description="Amount of jittering",
            min=0.0, max=1.0, soft_min=0.01, soft_max=1.0, default=1.0)

    antialias_gamma = FloatProperty(
            name="Antialias Gamma",
            description="POV-Ray compares gamma-adjusted values for super sampling. Antialias " \
                        "Gamma sets the Gamma before comparison",
            min=0.0, max=5.0, soft_min=0.01, soft_max=2.5, default=2.5)

    max_trace_level = IntProperty(
            name="Max Trace Level",
            description="Number of reflections/refractions allowed on ray path",
            min=1, max=256, default=5)

    photon_spacing = FloatProperty(
            name="Spacing",
            description="Average distance between photons on surfaces. half this get four times " \
                        "as many surface photons",
            min=0.001, max=1.000, soft_min=0.001, soft_max=1.000, default=0.005, precision=3)

    photon_max_trace_level = IntProperty(
            name="Max Trace Level",
            description="Number of reflections/refractions allowed on ray path",
            min=1, max=256, default=5)

    photon_adc_bailout = FloatProperty(
            name="ADC Bailout",
            description="The adc_bailout for photons. Use adc_bailout = " \
                        "0.01 / brightest_ambient_object for good results",
            min=0.0, max=1000.0, soft_min=0.0, soft_max=1.0, default=0.1, precision=3)

    photon_gather_min = IntProperty(
            name="Gather Min", description="Minimum number of photons gathered for each point",
            min=1, max=256, default=20)

    photon_gather_max = IntProperty(
            name="Gather Max", description="Maximum number of photons gathered for each point",
            min=1, max=256, default=100)

    radio_adc_bailout = FloatProperty(
            name="ADC Bailout",
            description="The adc_bailout for radiosity rays. Use " \
                        "adc_bailout = 0.01 / brightest_ambient_object for good results",
            min=0.0, max=1000.0, soft_min=0.0, soft_max=1.0, default=0.01, precision=3)

    radio_always_sample = BoolProperty(
            name="Always Sample",
            description="Only use the data from the pretrace step and not gather " \
                        "any new samples during the final radiosity pass",
            default=True)

    radio_brightness = FloatProperty(
            name="Brightness",
            description="Amount objects are brightened before being returned " \
                        "upwards to the rest of the system",
            min=0.0, max=1000.0, soft_min=0.0, soft_max=10.0, default=1.0)

    radio_count = IntProperty(
            name="Ray Count",
            description="Number of rays for each new radiosity value to be calculated " \
                        "(halton sequence over 1600)",
            min=1, max=10000, soft_max=1600, default=35)

    radio_error_bound = FloatProperty(
            name="Error Bound",
            description="One of the two main speed/quality tuning values, " \
                        "lower values are more accurate",
            min=0.0, max=1000.0, soft_min=0.1, soft_max=10.0, default=1.8)

    radio_gray_threshold = FloatProperty(
            name="Gray Threshold",
            description="One of the two main speed/quality tuning values, " \
                        "lower values are more accurate",
            min=0.0, max=1.0, soft_min=0, soft_max=1, default=0.0)

    radio_low_error_factor = FloatProperty(
            name="Low Error Factor",
            description="Just enough samples is slightly blotchy. Low error changes error " \
                        "tolerance for less critical last refining pass",
            min=0.0, max=1.0, soft_min=0.0, soft_max=1.0, default=0.5)

    # max_sample - not available yet
    radio_media = BoolProperty(
            name="Media", description="Radiosity estimation can be affected by media",
            default=False)

    radio_minimum_reuse = FloatProperty(
            name="Minimum Reuse",
            description="Fraction of the screen width which sets the minimum radius of reuse " \
                        "for each sample point (At values higher than 2% expect errors)",
            min=0.0, max=1.0, soft_min=0.1, soft_max=0.1, default=0.015, precision=3)

    radio_nearest_count = IntProperty(
            name="Nearest Count",
            description="Number of old ambient values blended together to " \
                        "create a new interpolated value",
            min=1, max=20, default=5)

    radio_normal = BoolProperty(
            name="Normals", description="Radiosity estimation can be affected by normals",
            default=False)

    radio_recursion_limit = IntProperty(
            name="Recursion Limit",
            description="how many recursion levels are used to calculate " \
                        "the diffuse inter-reflection",
            min=1, max=20, default=3)

    radio_pretrace_start = FloatProperty(
            name="Pretrace Start",
            description="Fraction of the screen width which sets the size of the " \
                        "blocks in the mosaic preview first pass",
            min=0.01, max=1.00, soft_min=0.02, soft_max=1.0, default=0.08)

    radio_pretrace_end = FloatProperty(
            name="Pretrace End",
            description="Fraction of the screen width which sets the size of the blocks " \
                        "in the mosaic preview last pass",
            min=0.001, max=1.00, soft_min=0.01, soft_max=1.00, default=0.04, precision=3)



class HaskRayRender(bpy.types.RenderEngine):
    bl_idname = 'HASKRAY_RENDER'
    bl_label = "HaskRay"
    DELAY = 0.5

    def _export(self, scene, povPath, renderImagePath):
        pass

    def _render(self, scene):
        pass

    def _cleanup(self):
        pass

    def render(self, scene):
        pass

def register():
    bpy.utils.register_module(__name__)

    bpy.types.Scene.hask = PointerProperty(type=RenderHaskRaySettingsScene)
    # bpy.types.Material.pov = PointerProperty(type=RenderPovSettingsMaterial)
    # bpy.types.Texture.pov = PointerProperty(type=RenderPovSettingsTexture)
    # bpy.types.Object.pov = PointerProperty(type=RenderPovSettingsObject)
    # bpy.types.Camera.pov = PointerProperty(type=RenderPovSettingsCamera)
    # bpy.types.Text.pov = PointerProperty(type=RenderPovSettingsText)


def unregister():
    bpy.utils.unregister_module(__name__)

    del bpy.types.Scene.hask
    # del bpy.types.Material.pov
    # del bpy.types.Texture.pov
    # del bpy.types.Object.pov
    # del bpy.types.Camera.pov
    # del bpy.types.Text.pov

if __name__ == "__main__":
    register()
