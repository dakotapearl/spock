package spock.network.behaviours;

import java.util.LinkedList;
import java.util.Queue;
import spock.network.core.NetworkNode;
import spock.network.signals.NetworkSignal;
import tools.errorChecking.Log;

/**
 * Storage of data and other nodes that are connected to this one
 * @author Loren Chorley
 */
public class StorageProcess extends NetworkBehaviour<StorageProcess> {

    protected Queue<NetworkSignal> signals;

    @Override
    public StorageProcess replicate(StorageProcess parentBehaviour) {
        return new StorageProcess();
    }

    @Override
    public void replaceInNode(NetworkNode node, StorageProcess behaviour) {
        node.storageProcess = behaviour;
    }

    public StorageProcess() {
        signals = new LinkedList<NetworkSignal>();
    }

    public void storeSignal(NetworkSignal signal) {
        synchronized (this) {
            signals.add(signal);

            Log.writeForMechanisms("StorageProcess: storing data: " + signal.toString());

            notifyAll();
        }
        parentNode.firingCondition.refresh(); // Not sure about this
    }

    public NetworkSignal retrieveSignal() throws InterruptedException {
        synchronized (this) {
            while (signals.size() == 0) wait();

            Log.writeForMechanisms("StorageProcess: retreiving data:" + signals.peek().toString());

            return signals.remove();
        }
    }

    public boolean hasSignals() {
        return signals.size() > 0;
    }

    @Override
    public void run() {}

    @Override
    public void pauseActivity() {}

    @Override
    public void resumeActivity() {}
}
