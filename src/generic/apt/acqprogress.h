// -*- mode: cpp; mode: fold -*-
// Description								/*{{{*/
// $Id: acqprogress.h,v 1.1.2.1 2002/02/23 14:50:07 daniel Exp $
/* ######################################################################

   Acquire Progress - Command line progress meter 
   
   ##################################################################### */
									/*}}}*/
#ifndef ACQPROGRESS_H
#define ACQPROGRESS_H

#include <apt-pkg/acquire.h>
#include <sigc++/slot.h>
#include <sigc++/trackable.h>

class download_signal_log;

class AcqTextStatus : public sigc::trackable
{
   unsigned int &ScreenWidth;
   char BlankLine[300];
   unsigned long ID;
   unsigned long Quiet;
   
  // We have to mirror the download_signal_log's information.
  //
  // A note: these values are only updated when Pulse() is called anyway.
  unsigned long TotalItems, CurrentItems;
  double CurrentCPS, TotalBytes, CurrentBytes;

   public:
   
   virtual void MediaChange(std::string Media,std::string Drive, download_signal_log &manager,
			    const sigc::slot1<void, bool> &k);
   virtual void IMSHit(pkgAcquire::ItemDesc &Itm, download_signal_log &manager);
   virtual void Fetch(pkgAcquire::ItemDesc &Itm, download_signal_log &manager);
   virtual void Done(pkgAcquire::ItemDesc &Itm, download_signal_log &manager);
   virtual void Fail(pkgAcquire::ItemDesc &Itm, download_signal_log &manager);
   virtual void Start(download_signal_log &manager);
   virtual void Stop(download_signal_log &manager, const sigc::slot0<void> &k);

   void Pulse(pkgAcquire *Owner, download_signal_log &manager,
	      const sigc::slot1<void, bool> &k);

   AcqTextStatus(unsigned int &ScreenWidth,unsigned int Quiet);

   virtual ~AcqTextStatus();
};

#endif
