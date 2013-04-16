package spock.dataaccess.ejb;

import javax.ejb.Remote;
import spock.dataaccess.ejb.interfaces.*;

/**
 *
 * @author Loren Chorley
 */
@Remote
public interface DataRequest {
    public boolean login(String username, String passwordHash);
    public boolean userVerified();
    public void sendPasswordResetEmail(String username);
    public ConfigurationFunctions Configuration();
    public NetworkFunctions Network();
    public ExperimentFunctions Experiment();
    public EnvironmentFunctions Environment();
    public UserFunctions User();
    public UserInterfaceFunctions UserInterface();
    public ConnectionFunctions Connection();
    public BehaviourFunctions Behaviour();
    public MetricFunctions Metric();
    public String returnTestString();
    public NewObjectInterface getNewObject();
}
