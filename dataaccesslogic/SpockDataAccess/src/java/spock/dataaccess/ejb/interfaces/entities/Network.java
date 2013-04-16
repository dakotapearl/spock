package spock.dataaccess.ejb.interfaces.entities;

import java.io.Serializable;
import java.util.Collection;

/**
 *
 * @author loren
 */
public interface Network extends Serializable {

    void addExperiment(Experiment experiment);
    void addMetric(Metric metric);
    void addNetworkInterface(NetworkInterface networkInterface);
    void addNetworkNode(NetworkNode networkNode);
    Collection<Experiment> getExperiments();
    String getId();
    Boolean getIsActive();
    Collection<Metric> getMetrics();
    Collection<NetworkInterface> getNetworkInterfaces();
    Collection<NetworkNode> getNetworkNodes();
    Collection<NetworkNode> getNodes();
    void setId(String id);
    void setIsActive(Boolean isActive);
    void setMetrics(Collection<Metric> metrics);
    void setNetworkInterfaces(Collection<NetworkInterface> networkInterfaces);
    void setNetworkNodes(Collection<NetworkNode> networkNodes);
    
}
