package spock.network.behaviours;

import spock.network.behaviours.independent.GeneFunctions;
import java.util.HashMap;
import spock.network.core.NetworkNode;

/**
 * @author Loren Chorley
 */
public class NodeProperties extends NetworkBehaviour<NodeProperties> {

    @Override
    public NodeProperties replicate(NodeProperties parentBehaviour) {
        return new NodeProperties();
    }

    @Override
    public void replaceInNode(NetworkNode node, NodeProperties behaviour) {
        node.nodeProperties = behaviour;
    }

    @SuppressWarnings({ "rawtypes", "unused" })
    protected class GSProperty {
            public String id;
            public Class datatype;
            public Object value;
            public GeneFunctions fns;
    }
    
    protected HashMap<String, GSProperty> properties;

    public NodeProperties() {
            properties = new HashMap<String, GSProperty>();
            
            // Move to genetic properties
            requestProperty("Mutation rate", Double.class, 0.0, new GeneFunctions() {

                    @Override
                    public Object mutate(Object originalValue, double mutationRate) {
                            return originalValue;
                    }

                    @Override
                    public Object getUpperBound() {
                            return 1.0;
                    }

                    @Override
                    public Object getLowerBound() {
                            return 0.0;
                    }

                    @Override
                    public int compare(Object thisValue, Object otherValue) {
                            if (Double.parseDouble(thisValue.toString()) == Double.parseDouble(otherValue.toString()))
                                    return 0;
                            else
                                    return Double.parseDouble(thisValue.toString()) > Double.parseDouble(otherValue.toString()) ? 1 : -1;
                    }

            });

    }

    @SuppressWarnings("rawtypes")
    public void requestProperty(String id, Class datatype, Object initialValue, GeneFunctions functions) {
            assert(id != null && datatype != null && initialValue != null && functions != null) : "requestGene parameters all non-null";

            GSProperty p = new GSProperty();
            p.id = id;
            p.datatype = datatype;
            p.value = initialValue;
            p.fns = functions;
            properties.put(id, p);

            assert(p.fns.compare(p.value, p.fns.getLowerBound()) >= 0 &&
                   p.fns.compare(p.value, p.fns.getUpperBound()) <= 0) : "New gene value (" + p.value 
                                              + ") is within bounds [" + p.fns.getLowerBound()
                                              + ", " + p.fns.getUpperBound() + "]";
    }

    public void setProperty(String id, Object value) {
            assert(properties.containsKey(id)) : "id not found";

            properties.get(id).value = value;

            assert(properties.get(id).fns.compare(properties.get(id).value, properties.get(id).fns.getLowerBound()) >= 0 &&
                   properties.get(id).fns.compare(properties.get(id).value, properties.get(id).fns.getUpperBound()) <= 0) : "Gene value is within bounds";
    }

    public Object getProperty(String id) {
            assert(properties.containsKey(id)) : "id not found";

            return properties.get(id).value;
    }

    @Override
    public void run() {}

    @Override
    public void pauseActivity() {}

    @Override
    public void resumeActivity() {}
    
}
