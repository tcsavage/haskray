import bpy

# bpy.context.active_object.data.polygons[0].vertices[0]
# bpy.context.active_object.data.vertices[0].co
mesh = bpy.context.active_object.data

def joinit(iterable, delimiter):
	it = iter(iterable)
	yield next(it)
	for x in it:
		yield delimiter
		yield x

def makeVec3(vector):
	return "(Vector3 (%f) (%f) (%f))" % (vector.x, vector.y, vector.z)

def makeVec2(vector):
	return "(Vector2 (%f) (%f))" % (vector.x, vector.y)

def makeVertex(vertex):
	pos = makeVec3(vertex.co)
	norm = makeVec3(vertex.normal)
	uv = "(Vector2 0 0)"
	vstr = "(Vertex %s %s %s)" % (pos, norm, uv)
	return vstr

tstrs = []

for p in mesh.polygons:
	if len(p.vertices) != 3:
		raise Exception("Not triangle enough: %d" % (len(p.vertices)))
	vstrs = []
	for v in p.vertices:
		vstrs.append(makeVertex(mesh.vertices[v]))
	tstr = "(Triangle " + " ".join(vstrs) + ")"
	tstrs.append(tstr)

meshstr = "(Mesh (Vector3 0 (-5) (-5)) [%s])" % (", ".join(tstrs))
print(meshstr)
