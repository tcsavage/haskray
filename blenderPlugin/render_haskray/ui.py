import bpy

# Use some of the existing buttons.
from bl_ui import properties_render
properties_render.RENDER_PT_render.COMPAT_ENGINES.add('HASKRAY_RENDER')
properties_render.RENDER_PT_dimensions.COMPAT_ENGINES.add('HASKRAY_RENDER')
# properties_render.RENDER_PT_antialiasing.COMPAT_ENGINES.add('HASKRAY_RENDER')
properties_render.RENDER_PT_shading.COMPAT_ENGINES.add('HASKRAY_RENDER')
properties_render.RENDER_PT_output.COMPAT_ENGINES.add('HASKRAY_RENDER')
del properties_render

# Use only a subset of the world panels
from bl_ui import properties_world
properties_world.WORLD_PT_preview.COMPAT_ENGINES.add('HASKRAY_RENDER')
properties_world.WORLD_PT_context_world.COMPAT_ENGINES.add('HASKRAY_RENDER')
properties_world.WORLD_PT_world.COMPAT_ENGINES.add('HASKRAY_RENDER')
properties_world.WORLD_PT_mist.COMPAT_ENGINES.add('HASKRAY_RENDER')
del properties_world

# Example of wrapping every class 'as is'
from bl_ui import properties_material
for member in dir(properties_material):
    subclass = getattr(properties_material, member)
    try:
        subclass.COMPAT_ENGINES.add('HASKRAY_RENDER')
    except:
        pass
del properties_material

from bl_ui import properties_data_mesh
for member in dir(properties_data_mesh):
    subclass = getattr(properties_data_mesh, member)
    try:
        subclass.COMPAT_ENGINES.add('HASKRAY_RENDER')
    except:
        pass
del properties_data_mesh

from bl_ui import properties_texture
for member in dir(properties_texture):
    subclass = getattr(properties_texture, member)
    try:
        subclass.COMPAT_ENGINES.add('HASKRAY_RENDER')
    except:
        pass
del properties_texture

from bl_ui import properties_data_camera
for member in dir(properties_data_camera):
    subclass = getattr(properties_data_camera, member)
    try:
        subclass.COMPAT_ENGINES.add('HASKRAY_RENDER')
    except:
        pass
del properties_data_camera

from bl_ui import properties_data_lamp
for member in dir(properties_data_lamp):
    subclass = getattr(properties_data_lamp, member)
    try:
        subclass.COMPAT_ENGINES.add('HASKRAY_RENDER')
    except:
        pass
del properties_data_lamp

from bl_ui import properties_particle as properties_particle
for member in dir(properties_particle):  # add all "particle" panels from blender
    subclass = getattr(properties_particle, member)
    try:
        subclass.COMPAT_ENGINES.add('HASKRAY_RENDER')
    except:
        pass
del properties_particle


class RenderButtonsPanel():
    bl_space_type = 'PROPERTIES'
    bl_region_type = 'WINDOW'
    bl_context = "render"
    # COMPAT_ENGINES must be defined in each subclass, external engines can add themselves here

    @classmethod
    def poll(cls, context):
        rd = context.scene.render
        return (rd.use_game_engine is False) and (rd.engine in cls.COMPAT_ENGINES)


class MaterialButtonsPanel():
    bl_space_type = 'PROPERTIES'
    bl_region_type = 'WINDOW'
    bl_context = "material"
    # COMPAT_ENGINES must be defined in each subclass, external engines can add themselves here

    @classmethod
    def poll(cls, context):
        mat = context.material
        rd = context.scene.render
        return mat and (rd.use_game_engine is False) and (rd.engine in cls.COMPAT_ENGINES)


class TextureButtonsPanel():
    bl_space_type = 'PROPERTIES'
    bl_region_type = 'WINDOW'
    bl_context = "texture"
    # COMPAT_ENGINES must be defined in each subclass, external engines can add themselves here

    @classmethod
    def poll(cls, context):
        tex = context.texture
        rd = context.scene.render
        return tex and (rd.use_game_engine is False) and (rd.engine in cls.COMPAT_ENGINES)


class ObjectButtonsPanel():
    bl_space_type = 'PROPERTIES'
    bl_region_type = 'WINDOW'
    bl_context = "object"
    # COMPAT_ENGINES must be defined in each subclass, external engines can add themselves here

    @classmethod
    def poll(cls, context):
        obj = context.object
        rd = context.scene.render
        return obj and (rd.use_game_engine is False) and (rd.engine in cls.COMPAT_ENGINES)


class CameraDataButtonsPanel():
    bl_space_type = 'PROPERTIES'
    bl_region_type = 'WINDOW'
    bl_context = "data"
    # COMPAT_ENGINES must be defined in each subclass, external engines can add themselves here

    @classmethod
    def poll(cls, context):
        cam = context.camera
        rd = context.scene.render
        return cam and (rd.use_game_engine is False) and (rd.engine in cls.COMPAT_ENGINES)


class TextButtonsPanel():
    bl_space_type = 'TEXT_EDITOR'
    bl_region_type = 'UI'
    bl_label = "P.O.V-Ray"
    # COMPAT_ENGINES must be defined in each subclass, external engines can add themselves here

    @classmethod
    def poll(cls, context):
        text = context.space_data
        rd = context.scene.render
        return text and (rd.use_game_engine is False) and (rd.engine in cls.COMPAT_ENGINES)


class RENDER_PT_haskray_export_settings(RenderButtonsPanel, bpy.types.Panel):
    bl_label = "Export Settings"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw(self, context):
        layout = self.layout

        scene = context.scene

        layout.active = scene.hask.max_trace_level
        split = layout.split()

        col = split.column()
        col.label(text="Command line switches:")
        col.prop(scene.hask, "command_line_switches", text="")
        split = layout.split()
        split.prop(scene.hask, "tempfiles_enable", text="OS Tempfiles")
        if not scene.hask.tempfiles_enable:
            split.prop(scene.hask, "deletefiles_enable", text="Delete files")

        if not scene.hask.tempfiles_enable:
            col = layout.column()
            col.prop(scene.hask, "scene_name", text="Name")
            col.prop(scene.hask, "scene_path", text="Path to files")
            #col.prop(scene.hask, "scene_path", text="Path to POV-file")
            #col.prop(scene.hask, "renderimage_path", text="Path to image")

            split = layout.split()
            split.prop(scene.hask, "indentation_character", text="Indent")
            if scene.hask.indentation_character == "2":
                split.prop(scene.hask, "indentation_spaces", text="Spaces")

            row = layout.row()
            row.prop(scene.hask, "comments_enable", text="Comments")
            row.prop(scene.hask, "list_lf_enable", text="Line breaks in lists")


class RENDER_PT_haskray_render_settings(RenderButtonsPanel, bpy.types.Panel):
    bl_label = "Render Settings"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw(self, context):
        layout = self.layout

        scene = context.scene

        layout.active = scene.hask.max_trace_level

        col = layout.column()

        col.label(text="Global Settings:")
        col.prop(scene.hask, "max_trace_level", text="Ray Depth")

        col.label(text="Global Photons:")
        col.prop(scene.hask, "photon_max_trace_level", text="Photon Depth")

        split = layout.split()

        col = split.column()
        col.prop(scene.hask, "photon_spacing", text="Spacing")
        col.prop(scene.hask, "photon_gather_min")

        col = split.column()
        col.prop(scene.hask, "photon_adc_bailout", text="Photon ADC")
        col.prop(scene.hask, "photon_gather_max")


class RENDER_PT_haskray_antialias(RenderButtonsPanel, bpy.types.Panel):
    bl_label = "Anti-Aliasing"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw_header(self, context):
        scene = context.scene

        self.layout.prop(scene.hask, "antialias_enable", text="")

    def draw(self, context):
        layout = self.layout

        scene = context.scene

        layout.active = scene.hask.antialias_enable

        row = layout.row()
        row.prop(scene.hask, "antialias_method", text="")
        row.prop(scene.hask, "jitter_enable", text="Jitter")

        split = layout.split()
        col = split.column()
        col.prop(scene.hask, "antialias_depth", text="AA Depth")
        sub = split.column()
        sub.prop(scene.hask, "jitter_amount", text="Jitter Amount")
        if scene.hask.jitter_enable:
            sub.enabled = True
        else:
            sub.enabled = False

        row = layout.row()
        row.prop(scene.hask, "antialias_threshold", text="AA Threshold")
        row.prop(scene.hask, "antialias_gamma", text="AA Gamma")


class RENDER_PT_haskray_radiosity(RenderButtonsPanel, bpy.types.Panel):
    bl_label = "Radiosity"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw_header(self, context):
        scene = context.scene

        self.layout.prop(scene.hask, "radio_enable", text="")

    def draw(self, context):
        layout = self.layout

        scene = context.scene

        layout.active = scene.hask.radio_enable

        split = layout.split()

        col = split.column()
        col.prop(scene.hask, "radio_count", text="Rays")
        col.prop(scene.hask, "radio_recursion_limit", text="Recursions")

        split.prop(scene.hask, "radio_error_bound", text="Error Bound")

        layout.prop(scene.hask, "radio_display_advanced")

        if scene.hask.radio_display_advanced:
            split = layout.split()

            col = split.column()
            col.prop(scene.hask, "radio_adc_bailout", slider=True)
            col.prop(scene.hask, "radio_gray_threshold", slider=True)
            col.prop(scene.hask, "radio_low_error_factor", slider=True)
            col.prop(scene.hask, "radio_pretrace_start", slider=True)

            col = split.column()
            col.prop(scene.hask, "radio_brightness")
            col.prop(scene.hask, "radio_minimum_reuse", text="Min Reuse")
            col.prop(scene.hask, "radio_nearest_count")
            col.prop(scene.hask, "radio_pretrace_end", slider=True)

            split = layout.split()

            col = split.column()
            col.label(text="Estimation Influence:")
            col.prop(scene.hask, "radio_media")
            col.prop(scene.hask, "radio_normal")

            split.prop(scene.hask, "radio_always_sample")


class RENDER_PT_haskray_media(RenderButtonsPanel, bpy.types.Panel):
    bl_label = "Atmosphere Media"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw_header(self, context):
        scene = context.scene

        self.layout.prop(scene.hask, "media_enable", text="")

    def draw(self, context):
        layout = self.layout

        scene = context.scene

        layout.active = scene.hask.media_enable

        row = layout.row()
        row.prop(scene.hask, "media_samples", text="Samples")
        row.prop(scene.hask, "media_color", text="")

##class RENDER_PT_haskray_baking(RenderButtonsPanel, bpy.types.Panel):
##    bl_label = "Baking"
##    COMPAT_ENGINES = {'HASKRAY_RENDER'}
##
##    def draw_header(self, context):
##        scene = context.scene
##
##        self.layout.prop(scene.hask, "baking_enable", text="")
##
##    def draw(self, context):
##        layout = self.layout
##
##        scene = context.scene
##        rd = scene.render
##
##        layout.active = scene.hask.baking_enable


class MATERIAL_PT_haskray_mirrorIOR(MaterialButtonsPanel, bpy.types.Panel):
    bl_label = "IOR Mirror"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw_header(self, context):
        scene = context.material

        self.layout.prop(scene.hask, "mirror_use_IOR", text="")

    def draw(self, context):
        layout = self.layout

        mat = context.material
        layout.active = mat.hask.mirror_use_IOR

        if mat.hask.mirror_use_IOR:
            col = layout.column()
            col.alignment = 'CENTER'
            col.label(text="The current Raytrace ")
            col.label(text="Transparency IOR is: " + str(mat.raytrace_transparency.ior))


class MATERIAL_PT_haskray_metallic(MaterialButtonsPanel, bpy.types.Panel):
    bl_label = "metallic Mirror"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw_header(self, context):
        scene = context.material

        self.layout.prop(scene.hask, "mirror_metallic", text="")

    def draw(self, context):
        layout = self.layout

        mat = context.material
        layout.active = mat.hask.mirror_metallic


class MATERIAL_PT_haskray_fade_color(MaterialButtonsPanel, bpy.types.Panel):
    bl_label = "Interior Fade Color"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw_header(self, context):
        mat = context.material

        self.layout.prop(mat.hask, "interior_fade_color", text="")

    def draw(self, context):
        # layout = self.layout
        # mat = context.material
        # layout.active = mat.hask.interior_fade_color
        pass


class MATERIAL_PT_haskray_conserve_energy(MaterialButtonsPanel, bpy.types.Panel):
    bl_label = "conserve energy"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw_header(self, context):
        mat = context.material

        self.layout.prop(mat.hask, "conserve_energy", text="")

    def draw(self, context):
        layout = self.layout

        mat = context.material
        layout.active = mat.hask.conserve_energy


class MATERIAL_PT_haskray_iridescence(MaterialButtonsPanel, bpy.types.Panel):
    bl_label = "iridescence"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw_header(self, context):
        mat = context.material

        self.layout.prop(mat.hask, "irid_enable", text="")

    def draw(self, context):
        layout = self.layout

        mat = context.material
        layout.active = mat.hask.irid_enable

        if mat.hask.irid_enable:
            col = layout.column()
            col.prop(mat.hask, "irid_amount", slider=True)
            col.prop(mat.hask, "irid_thickness", slider=True)
            col.prop(mat.hask, "irid_turbulence", slider=True)


class MATERIAL_PT_haskray_caustics(MaterialButtonsPanel, bpy.types.Panel):
    bl_label = "Caustics"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw_header(self, context):
        mat = context.material

        self.layout.prop(mat.hask, "caustics_enable", text="")

    def draw(self, context):

        layout = self.layout

        mat = context.material
        layout.active = mat.hask.caustics_enable

        if mat.hask.caustics_enable:
            col = layout.column()
            col.prop(mat.hask, "refraction_type")

            if mat.hask.refraction_type == "1":
                col.prop(mat.hask, "fake_caustics_power", slider=True)
            elif mat.hask.refraction_type == "2":
                col.prop(mat.hask, "photons_dispersion", slider=True)
                col.prop(mat.hask, "photons_dispersion_samples", slider=True)
            col.prop(mat.hask, "photons_reflection")

            if mat.hask.refraction_type == "0" and not mat.hask.photons_reflection:
                col = layout.column()
                col.alignment = 'CENTER'
                col.label(text="Caustics override is on, ")
                col.label(text="but you didn't chose any !")


class MATERIAL_PT_haskray_replacement_text(MaterialButtonsPanel, bpy.types.Panel):
    bl_label = "Custom POV Code"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw(self, context):
        layout = self.layout

        mat = context.material

        col = layout.column()
        col.label(text="Replace properties with:")
        col.prop(mat.hask, "replacement_text", text="")


class TEXTURE_PT_haskray_tex_gamma(TextureButtonsPanel, bpy.types.Panel):
    bl_label = "Image Gamma"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw_header(self, context):
        tex = context.texture

        self.layout.prop(tex.hask, "tex_gamma_enable", text="")

    def draw(self, context):
        layout = self.layout

        tex = context.texture

        layout.active = tex.hask.tex_gamma_enable
        layout.prop(tex.hask, "tex_gamma_value", text="Gamma Value")


class TEXTURE_PT_haskray_replacement_text(TextureButtonsPanel, bpy.types.Panel):
    bl_label = "Custom POV Code"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw(self, context):
        layout = self.layout

        tex = context.texture

        col = layout.column()
        col.label(text="Replace properties with:")
        col.prop(tex.hask, "replacement_text", text="")


class OBJECT_PT_haskray_obj_importance(ObjectButtonsPanel, bpy.types.Panel):
    bl_label = "POV-Ray"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw(self, context):
        layout = self.layout

        obj = context.object

        col = layout.column()
        col.label(text="Radiosity:")
        col.prop(obj.hask, "importance_value", text="Importance")
        col.label(text="Photons:")
        col.prop(obj.hask, "collect_photons", text="Receive Photon Caustics")
        if obj.hask.collect_photons:
            col.prop(obj.hask, "spacing_multiplier", text="Photons Spacing Multiplier")


class OBJECT_PT_haskray_replacement_text(ObjectButtonsPanel, bpy.types.Panel):
    bl_label = "Custom POV Code"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw(self, context):
        layout = self.layout

        obj = context.object

        col = layout.column()
        col.label(text="Replace properties with:")
        col.prop(obj.hask, "replacement_text", text="")


class CAMERA_PT_haskray_cam_dof(CameraDataButtonsPanel, bpy.types.Panel):
    bl_label = "POV-Ray Depth Of Field"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw_header(self, context):
        cam = context.camera

        self.layout.prop(cam.hask, "dof_enable", text="")

    def draw(self, context):
        layout = self.layout

        cam = context.camera

        layout.active = cam.hask.dof_enable

        layout.prop(cam.hask, "dof_aperture")

        split = layout.split()

        col = split.column()
        col.prop(cam.hask, "dof_samples_min")
        col.prop(cam.hask, "dof_variance")

        col = split.column()
        col.prop(cam.hask, "dof_samples_max")
        col.prop(cam.hask, "dof_confidence")


class CAMERA_PT_haskray_replacement_text(CameraDataButtonsPanel, bpy.types.Panel):
    bl_label = "Custom POV Code"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw(self, context):
        layout = self.layout

        cam = context.camera

        col = layout.column()
        col.label(text="Replace properties with:")
        col.prop(cam.hask, "replacement_text", text="")


class TEXT_PT_haskray_custom_code(TextButtonsPanel, bpy.types.Panel):
    bl_label = "P.O.V-Ray"
    COMPAT_ENGINES = {'HASKRAY_RENDER'}

    def draw(self, context):
        layout = self.layout

        text = context.space_data.text
        if text:
            layout.prop(text.hask, "custom_code", text="Add as POV code")
