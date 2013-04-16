package spock.network.behaviours;

import java.util.Observable;
import java.io.Serializable;
import spock.network.core.NetworkNode;
import spock.network.signals.NetworkSignal;
import tools.concurrency.Port;
import tools.errorChecking.Assert;
import tools.errorChecking.Log;

/**
 * @author Loren Chorley
 */
public class InputProcess extends NetworkBehaviour<InputProcess>  {
	
    private boolean isPaused = false;
    
    private Port<NetworkSignal> port;

    @Override
    public InputProcess replicate(InputProcess parentBehaviour) {
        return new InputProcess();
    }

    @Override
    public void replaceInNode(NetworkNode node, InputProcess behaviour) {
        node.inputProcess = behaviour;
    }
    
    public InputProcess() {
        port = new Port<NetworkSignal>();
        this.setThreadEnabled(true);
    }

    // note that here the sending node is ignored, but may be used if this class is extended.
    public void acceptSignal(NetworkSignal signal, NetworkNode sender) {
        Log.writeForMechanisms("InputProcess: Accepted signal with datum: " + signal.toString());

        port.send(signal);
    }

    @Override
    public void run() {
        NetworkSignal receivedSignal;
        NetworkSignal ProcessedSignal;

        while (true) {
            
            if (isPaused) {
                // TODO figure out how to block here
            }
            
            try {
                Log.writeForMechanisms("InputProcess: Waiting for port to receive");
                receivedSignal = port.receive();
                Assert.AssertTrue("dataCell is always not null", receivedSignal != null);
                Log.writeForMechanisms("InputProcess: Received signal from port with datum: " + receivedSignal.toString());

                // Process
                ProcessedSignal = parentNode.dataProcessing.processData(receivedSignal);
                Log.writeForMechanisms("InputProcess: Processed data into: " + ProcessedSignal.toString());

                // Store
                Log.writeForMechanisms("InputProcess: Sending data to be stored");
                parentNode.storageProcess.storeSignal(ProcessedSignal);

                // Add energy received (originalEnergyValue)
                // Plus modify energy level to account for energy lost or gained in processing
                //parentNode.energyEconomics.offsetEnergy(2 * receivedSignal.getData().getEnergy() - ProcessedSignal.getEnergy());
                Log.writeForMechanisms("InputProcess: Added energy from signal");

            } catch (InterruptedException e) {/*Ignore and repeat*/}
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
