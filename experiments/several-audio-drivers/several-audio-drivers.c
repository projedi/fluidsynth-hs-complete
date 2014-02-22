// A modified example.c

#include <fluidsynth.h>

#ifdef WIN32
#include <windows.h>
void sleep(int t) { Sleep(t * 1000); }
#else
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#endif

int main(int argc, char** argv)
{
  fluid_settings_t* settings;
  fluid_synth_t* synth;
  fluid_audio_driver_t* adriver1, *adriver2, *adriver3;
  int sfont_id;
  int i, key;
  /* Create the settings. */
  settings = new_fluid_settings();
  /* Change the settings if necessary*/
  /* Create the synthesizer. */
  synth = new_fluid_synth(settings);
  /* Create the audio driver. The synthesizer starts playing as soon
     as the driver is created. */
  adriver1 = new_fluid_audio_driver(settings, synth);
  fluid_settings_setstr(settings, "audio.driver", "pulseaudio");
  adriver2 = new_fluid_audio_driver(settings, synth);
  fluid_settings_setstr(settings, "audio.driver", "alsa");
  adriver3 = new_fluid_audio_driver(settings, synth);
  /* Load a SoundFont and reset presets (so that new instruments
   * get used from the SoundFont) */
  sfont_id = fluid_synth_sfload(synth, "/usr/share/soundfonts/SGM-V2.01.sf2", 1);
  /* Initialize the random number generator */
  srand(time(NULL));
  for (i = 0; i < 12; i++) {
    /* Generate a random key */
    key = 60 + (int) (12.0f * rand() / (float) RAND_MAX);
    /* Play a note */
    fluid_synth_noteon(synth, 0, key, 80);
    /* Sleep for 1 second */
    sleep(1);
    /* Stop the note */
    fluid_synth_noteoff(synth, 0, key);
  }
  /* Clean up */
  delete_fluid_audio_driver(adriver1);
  delete_fluid_audio_driver(adriver2);
  delete_fluid_audio_driver(adriver3);
  delete_fluid_synth(synth);
  delete_fluid_settings(settings);
  return 0;
}
