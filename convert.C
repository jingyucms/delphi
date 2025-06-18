#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
#include <TFile.h>
#include "TTree.h"
#include "TVector3.h"

//DataProcessing dependencies
#include "DataProcessing/include/particleData.h"
#include "DataProcessing/include/eventSelection.h"
#include "DataProcessing/include/eventData.h"
#include "DataProcessing/include/thrustTools.h"
#include "DataProcessing/include/sphericityTools.h"

using namespace std;

void convert(const char* inputFileName, const char* outputFileName,
             bool verbose=false)
{
    // Input and output files
    std::ifstream infile(inputFileName);
    if (!infile.is_open()) {
        std::cerr << "Error: Cannot open input file " << inputFileName << std::endl;
        return;
    }

    TFile outFile(outputFileName, "RECREATE");
    if (outFile.IsZombie()) {
        std::cerr << "Error: Cannot create output file " << outputFileName << std::endl;
        return;
    }

    // Create a TTree to store event-based data
    TTree * out_t= new TTree("t", "t");
    particleData    out_pData;
    eventData       out_eData;

    // Variables for event information and particle data
    float emf[particleData::nMaxPart];
    float hpc[particleData::nMaxPart];
    float hac[particleData::nMaxPart];
    float stic[particleData::nMaxPart];
    float lock[particleData::nMaxPart];
    out_t->Branch("EMF", emf);
    out_t->Branch("HPC", hpc);
    out_t->Branch("HAC", hac);
    out_t->Branch("STIC", stic);
    out_t->Branch("LOCK", lock);

    // register branches
    do_chThrust           = false;
    do_neuThrust          = false;
    do_thrustCorr         = false;
    do_thrustCorrInverse  = false;
    do_thrustMissP        = true;
    out_pData.SetBranchWrite(out_t, 1);
    out_eData.SetBranchWrite(out_t, 0);

    // Process the log file
    std::string line;
    bool doParticle=0;
    int iEvent=0;
    int nParticleNoCut = 0; int nParticle = 0;
    int nParticleHP = 0; int nChargedParticle = 0; int nChargedParticleHP = 0;
    float energy = 0;
    TVector3 netP(0, 0, 0);
    TVector3 netChargedP(0, 0, 0);
    while (std::getline(infile, line)) {
        if (line.find("HAPPY CHECK: EVENT") != std::string::npos) {
            // Locate and extract the part after "HAPPY CHECK: EVENT   :"
            if(iEvent%1000 == 0)  cout<<"\r event processed : "<<iEvent<<flush;
            size_t colonPos = line.find('EVENT   :');
            if (colonPos != std::string::npos) {
                line = line.substr(colonPos+16);
                std::istringstream iss(line);
                // std::cout << "Data substring: '" << line << "'" << std::endl;
                char colon;
                iss >> out_pData.RunNo >> colon >> out_pData.EventNo;
                if (verbose) std::cout << "Run: " << out_pData.RunNo << ", Colon: " << colon << ", Event: " << out_pData.EventNo << std::endl;
            }
            out_pData.year = 1998; // temp //
            // out_pData.subDir = -999;
            // out_pData.process = -999;
            out_pData.source = 70; // temp //
            out_pData.isMC = false;
            out_pData.isOnres = false;
            // out_pData.uniqueID = 0;
            // out_pData.Energy = 0;
            // out_pData.bFlag = -999;
            out_pData.particleWeight = 1;
            // out_pData.bx = -999;
            // out_pData.by = -999;
            // out_pData.ebx = -999;
            // out_pData.eby = -999;

            nParticle = 0;
            nParticleHP = 0;
            nChargedParticle = 0;
            nChargedParticleHP = 0;
            netP = TVector3(0, 0, 0);
	    netChargedP = TVector3(0, 0, 0);

        } else if (line.find("CHECK: ECM") != std::string::npos) {
	    std::istringstream iss(line);
	    std::string dummy;
            iss >> dummy >> dummy >> energy;
	    if (verbose) std::cout <<"ECM: "<< energy << std::endl;
	    out_pData.Energy = energy;
	} else if (line.find("CHECK: TRACKS") != std::string::npos) {
            std::istringstream iss(line);
            std::string dummy;
            iss >> dummy >> dummy >> nParticleNoCut;
            if (verbose) std::cout <<"get "<< nParticleNoCut << " particles" << std::endl;
            doParticle=1;
        } else if (!line.empty() && doParticle==1) {
            std::istringstream iss(line);
            int particleNumber, eid, conversion, muid;
            float q, pxVal, pyVal, pzVal, eVal, emfVal, hpcVal, hacVal, sticVal, lockVal, d0, z0, length;
            iss >> particleNumber >> q >> pxVal >> pyVal >> pzVal >> eVal >> emfVal >> hpcVal >> hacVal >> sticVal >> lockVal >> d0 >> z0 >> length >> eid >> conversion >> muid;

            TLorentzVector temp(pxVal, pyVal, pzVal, eVal);

            ///// veto the beam background and other backgrounds /////
            bool pass(1);
            pass = !lockVal; // bad particle
            ///// veto the beam background and other backgrounds /////

            if (pass)
            {
                if (verbose) std::cout <<"do particle: " << nParticle << "-th (" << particleNumber << ")" <<std::endl;
                netP -= TVector3(pxVal, pyVal, pzVal);
		if (abs(q) > 0.5) {netChargedP -= TVector3(pxVal, pyVal, pzVal);}

                out_pData.px[nParticle] = pxVal;
                out_pData.py[nParticle] = pyVal;
                out_pData.pz[nParticle] = pzVal;
                emf[nParticle] = emfVal;
                hpc[nParticle] = hpcVal;
                hac[nParticle] = hacVal;
                stic[nParticle] = sticVal;
                lock[nParticle] = lockVal;
                out_pData.pt[nParticle] = temp.Pt();
                out_pData.pmag[nParticle]   = temp.P();
                out_pData.rap[nParticle]    = temp.Rapidity();
                out_pData.eta[nParticle]    = temp.Eta();
                out_pData.theta[nParticle]  = temp.Theta();
                out_pData.phi[nParticle]    = temp.Phi();
                out_pData.mass[nParticle]   = temp.M();
                out_pData.charge[nParticle] = q;
                out_pData.pwflag[nParticle] = (out_pData.charge[nParticle]!=0)? 0: 4; // charged = 0, neutral = 4
		out_pData.d0[nParticle] = d0;
		out_pData.z0[nParticle] = z0;
		out_pData.ntpc[nParticle] = (out_pData.charge[nParticle]!=0)? 7: 0;
		out_pData.weight[nParticle] = length; // use this variable to store track length for DELPHI
		out_pData.eleId[nParticle] = eid;
		out_pData.muId[nParticle] = muid;
		out_pData.conversion[nParticle] = conversion;

		// follow the same definition in eventSelection.h
		if (out_pData.pwflag[nParticle]<=2) {
		  out_pData.highPurity[nParticle]= out_pData.pwflag[nParticle]<=2 && temp.Pt() >= 0.2;
		} else if (out_pData.pwflag[nParticle]==4) {
		  out_pData.highPurity[nParticle]= out_pData.pwflag[nParticle]==4 && temp.Pt() >= 0.4;
		}

                if(out_pData.pwflag[nParticle]<=2) {
                    nChargedParticle++;
                    if (out_pData.highPurity[nParticle]) nChargedParticleHP++;
                }
                if (out_pData.highPurity[nParticle]) nParticleHP++;
                nParticle++;
            }

            if (particleNumber==nParticleNoCut)  {
                out_pData.nParticle       = nParticle;
                out_eData.nChargedParticle  = nChargedParticle;
                out_eData.nParticleHP       = nParticleHP;
                out_eData.nChargedParticleHP= nChargedParticleHP;
                doParticle=0;
                if (verbose) std::cout <<"End reading particles, recording " << out_pData.nParticle << " particles." <<std::endl;

                out_eData.missP = netP.Mag();
                out_eData.missPt = netP.Perp();
                out_eData.missTheta = netP.Theta();
                out_eData.missPhi = netP.Phi();

		out_eData.missChargedP = netChargedP.Mag();
		out_eData.missChargedPt = netChargedP.Perp();
		out_eData.missChargedTheta = netChargedP.Theta();
		out_eData.missChargedPhi = netChargedP.Phi();

                TVector3 thrust             = getThrust(out_pData.nParticle, out_pData.px, out_pData.py, out_pData.pz, THRUST::OPTIMAL);
                TVector3 thrustWithMissP    = getThrust(out_pData.nParticle, out_pData.px, out_pData.py, out_pData.pz, THRUST::OPTIMAL,false,false,NULL,true,out_pData.pwflag);

                out_eData.Thrust        = thrust.Mag();
                out_eData.TTheta        = thrust.Theta();
                out_eData.TPhi          = thrust.Phi();
                out_eData.ThrustWithMissP   = thrustWithMissP.Mag();
                out_eData.TThetaWithMissP   = thrustWithMissP.Theta();
                out_eData.TPhiWithMissP     = thrustWithMissP.Phi();
                if ( do_thrustMissP ) {
                        setThrustVariables(&out_pData, &out_eData, TVector3(),
                                TVector3(), TVector3(), TVector3(), TVector3(),
                                thrustWithMissP);
                }

                Sphericity spher = Sphericity(out_pData.nParticle,
                                  out_pData.px,
                                  out_pData.py,
                                  out_pData.pz,
                                  out_pData.pwflag,
                                  false);
                spher.setTree(&out_eData);

                eventSelection eSelection;
                eSelection.setEventSelection(&out_pData, &out_eData);

// The following lines are redundant
//                Float_t TotalChgEnergy = 0;
//                Int_t totalChgParticles = 0;
//                for(Int_t pI = 0; pI < out_pData.nParticle; ++pI)
//                {
//                    if( out_pData.charge[pI]==0) continue;
//                    if(!out_pData.highPurity[pI]) continue;
//                    TotalChgEnergy += TMath::Sqrt(out_pData.pmag[pI]*out_pData.pmag[pI] + out_pData.mass[pI]*out_pData.mass[pI]);
//                    totalChgParticles += 1;
//                }
//                eSelection.passesTotalChgEnergyMin = TotalChgEnergy >= 15;
//                eSelection.passesNeuNch = out_pData.nParticle-totalChgParticles+out_eData.nChargedParticleHP >= 13;
                if (verbose)
                {
		  //printf("out_pData.nParticle: %d\n", out_pData.nParticle);
                  //printf("totalChgParticles: %d\n", totalChgParticles);
                  //printf("out_eData.nChargedParticleHP: %d\n", out_eData.nChargedParticleHP);
                    printf("eSelection.getPassesNeuNch(): %o\n", eSelection.getPassesNeuNch());
                    printf("eSelection.getPassesSTheta(): %o\n", eSelection.getPassesSTheta());
                    printf("eSelection.getPassesTotalChgEnergyMin(): %o\n", eSelection.getPassesTotalChgEnergyMin());
                    //printf("TotalChgEnergy: %.3f, NeuNch: %d\n", TotalChgEnergy, out_pData.nParticle-totalChgParticles+out_eData.nChargedParticleHP);
                    printf("out_eData.nChargedParticleHP: %d\n", out_eData.nChargedParticleHP);
                }
		out_eData.passesTotalChgEnergyMin = eSelection.getPassesTotalChgEnergyMin();
		out_eData.passesNeuNch = eSelection.getPassesNeuNch();
		out_eData.passesNTrkMin = eSelection.getPassesNTrkMin();
		out_eData.passesSTheta = eSelection.getPassesSTheta();
                out_eData.passesBELLE = eSelection.getPassesNeuNch() && \
		  eSelection.getPassesSTheta() &&			\
		  eSelection.getPassesTotalChgEnergyMin() &&		\
		  eSelection.getPassesNTrkMin();
                out_eData.passesISR = eSelection.getPassesISR();
                out_eData.passesWW = eSelection.getPassesWW();
		out_eData.Mvis = eSelection.getMvis();
		out_eData.sPrime = eSelection.getsPrime();
		out_eData.d2 = eSelection.getd2();
		out_eData.cW = eSelection.getcW();
                // if(!out_eData.passesBELLE || !out_eData.passesISR) continue;

                out_t->Fill();
                iEvent ++;
                // if (iEvent==10) break;
                if (verbose) std::cout <<"fill"<<std::endl;
            }
        }
    }
    printf("Total %d events recorded", iEvent);

    // Write the tree to the output file
    out_t->Write();
    outFile.Close();
    infile.close();

    std::cout << "Data successfully converted to " << outputFileName << std::endl;
}


int main(int argc, char const *argv[])
{
    convert(argv[1], argv[2]);
    return 0;
}
