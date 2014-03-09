#include <EGL/egl.h>
#include <GLES/gl.h>

static void
draw(int frame)
{
   static const GLfloat verts[3][2] = {
      { -1, -1 },
      {  1, -1 },
      {  0,  1 }
   };
   static const GLfloat colors[3][4] = {
      { 1, 0, 0, 1 },
      { 0, 1, 0, 1 },
      { 0, 0, 1, 1 }
   };
   GLfloat view_rotz = (GLfloat) frame;

   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

   glPushMatrix();
   glRotatef(view_rotz, 0, 0, 1);

   glVertexPointer(2, GL_FLOAT, 0, verts);
   glColorPointer(4, GL_FLOAT, 0, colors);

   glEnableClientState(GL_VERTEX_ARRAY);
   glEnableClientState(GL_COLOR_ARRAY);

   /* draw triangle */
   glDrawArrays(GL_TRIANGLES, 0, 3);

   glDisableClientState(GL_VERTEX_ARRAY);
   glDisableClientState(GL_COLOR_ARRAY);

   glPopMatrix();
}

static void
egl_present(void)
{
    cloglure_swap_buffers();
}


int
main(int argc, char **argv)
{
    char fbdev[] = "/dev/graphics/fb0";
    //    struct fb_var_screeninfo vinfo;
    int fd, frame;
    fd = cloglure_start(fbdev);
    
    for (frame = 0; frame <= 180; frame++) {
	draw(frame);
	egl_present();
    }
    cloglure_stop(fd);
    
    return 0;
}
