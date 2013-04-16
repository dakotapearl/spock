package spock.network.behaviours;

import tools.errorChecking.Log;
import spock.network.core.NetworkNode;
import spock.network.signals.NetworkSignal;

/**
 * @author Loren Chorley
 */
public class OutputProcess extends NetworkBehaviour<OutputProcess> {
    
    private boolean isPaused = false;
    
    public OutputProcess() {
        this.setThreadEnabled(true);
    }
    
    @Override
    public OutputProcess replicate(OutputProcess parentBehaviour) {
        return new OutputProcess();
    }

    @Override
    public void replaceInNode(NetworkNode node, OutputProcess behaviour) {
        node.outputProcess = behaviour;
    }
	
    @Override
    public void run() {
            NetworkSignal signal;
            NetworkNode target;

            while (true) {

                    Log.writeForMechanisms("OutputProcess: waiting for ready to fire");

                    // wait until ready to fire (blocking oeration)
                    // TODO figure out how to block when paused here
                    if (parentNode.firingCondition.waitUntilReady() && !isPaused) {

                            Log.writeForMechanisms("OutputProcess: Firing Condition set");

                            // Select signal to fire
                            signal = parentNode.transmissionContent.selectContent();
                            Log.writeForMechanisms("OutputProcess: Selected signal");

                            // Select target
                            target = parentNode.targetSelection.selectTarget(signal);
                            Log.writeForMechanisms("OutputProcess: Selected target");

                            // Fire
                            Log.write("Node firing signal with datum: " + signal.toString());
                            target.acceptSignal(signal, parentNode);

                            Log.writeForMechanisms("OutputProcess: Sent signal");

                            // Network observable code
                            //parentNode.networkDomain.getNetwork().nodeactivation();
                            //parentNode.networkDomain.getNetwork().interfaceObservables.get("Latest transfer").updateInterface(Integer.toString(parentNode.getID()) + " -> " + Integer.toString(target.getID()));

                    }

            }

    }

    @Override
    public void pauseActivity() {
        isPaused = true;
    }

    @Override
    public void resumeActivity() {
        isPaused = false;
    }

}
